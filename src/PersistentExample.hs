{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module PersistentExample where

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Concurrent
import           Control.Monad
import           Data.Kind (Type)
import           Data.List hiding (insert)
import           Data.Pool
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Database.Sqlite hiding (step)
import           Control.Monad.Logger
import           Database.Persist.Class.PersistEntity
import           GHC.Generics
import           Test.QuickCheck
import           Test.StateMachine
import           Test.StateMachine.Types

share [mkPersist sqlSettings, mkSave "entityDefs"] [persistLowerCase|
Person
    name String
    age Int
    Primary name
    deriving Show
    deriving Eq

Car
    cid Int
    color String
    owner PersonId -- foreign key
    UniqueCid cid
    deriving Show
    deriving Eq
|]

names :: [String]
names = ["John", "Stevan", "Kostas", "Curry", "Robert"]

-- Boilerplate

instance Arbitrary Person where
    arbitrary = Person <$> elements names
                       <*> suchThat arbitrary (>0)

instance Arbitrary Car where
    arbitrary = Car <$> arbitrary
                    <*> arbitrary
                    <*>(PersonKey <$> elements names)

instance Arbitrary (SelectOpt Person) where
    arbitrary = oneof [ OffsetBy <$> suchThat arbitrary (>=0)
                      , LimitTo <$> suchThat arbitrary (>=0)
                      , return $ Asc PersonName
                      , return $ Asc PersonAge
                      , return $ Desc PersonName
                      , return $ Desc PersonAge
                      ]

instance Arbitrary (SelectOpt Car) where
    arbitrary = oneof [ OffsetBy <$> suchThat arbitrary (>=0)
                      , LimitTo <$> suchThat arbitrary (>=0)
                      , return $ Asc CarCid
                      , return $ Asc CarColor
                      , return $ Asc CarOwner
                      , return $ Desc CarCid
                      , return $ Desc CarColor
                      , return $ Desc CarOwner
                      ]

instance Arbitrary PersistFilter where
    arbitrary = elements [Eq, Ne, Gt, Lt, Ge, Le, In, NotIn]

instance Arbitrary (Filter Person) where
    arbitrary = do
      ff <- arbitrary
      let genName = do
            name <- elements names
            return Filter {
                  filterField = PersonName
                , filterValue = FilterValue name
                , filterFilter = ff
                }
      let genAge = do
            age <- suchThat arbitrary (>=0)
            return Filter {
                  filterField = PersonAge
                , filterValue = FilterValue age
                , filterFilter = ff
                }
      oneof [genName, genAge]

instance Arbitrary (Filter Car) where
    arbitrary = do
      ff <- arbitrary
      let genCid = do
            n <- arbitrary
            return Filter {
                  filterField = CarCid
                , filterValue = FilterValue n
                , filterFilter = ff
                }
      let genColor = do
            color <- arbitrary
            return Filter {
                  filterField = CarColor
                , filterValue = FilterValue color
                , filterFilter = ff
                }
      let genOwner = do
            name <- elements names
            return Filter {
                  filterField = CarOwner
                , filterValue = FilterValue (PersonKey name)
                , filterFilter = ff
                }
      oneof [genCid, genColor, genOwner]

data TableEntity = TPerson Person
                 | TCar Car
                 deriving Eq

data TableTag = PersonTag | CatTag

instance Arbitrary TableEntity where
    arbitrary = do
        frequency [ (1, TPerson <$> arbitrary)
                  , (1, TCar <$> arbitrary)
                  ]

migrations :: [(Migration, TableTag)]
migrations = [
      (migrate entityDefs $ entityDef (Nothing :: Maybe Car), PersonTag)
    , (migrate entityDefs $ entityDef (Nothing :: Maybe Person), CatTag)
    ]

data TableSelectOpts record = TableSelect [Filter record] [SelectOpt record]
data TableSelect = SPerson (TableSelectOpts Person) | SCar (TableSelectOpts Car)

instance (Arbitrary (Filter record), Arbitrary (SelectOpt record))
    => Arbitrary (TableSelectOpts record) where
        arbitrary = do
            fop <- listOf arbitrary
            sop <- listOf arbitrary
            return $ TableSelect fop sop

instance Arbitrary TableSelect where
    arbitrary = frequency [
                  (1, SPerson <$> arbitrary)
                , (1, SCar <$> arbitrary)
                ]

data Cmd =
      Migrate (Migration, TableTag)
    | Insert TableEntity
    | SelectList TableSelect

instance Arbitrary Cmd where
    arbitrary = do
        frequency [ (1, Migrate <$> elements migrations)
                  , (3, Insert <$> arbitrary)
                  , (3, SelectList <$> arbitrary)
                  ]

newtype At t (r :: Type -> Type) = At {unAt :: t}
    deriving (Generic)

-- | Alias for 'At'
type (:@) t r = At t r

data Success =
      Unit ()
    | List [TableEntity]

data Model (r :: Type -> Type) = Model
    { persons :: Maybe [Person]
    , cars    :: Maybe [Car]
    } deriving (Generic, Show)

initModelImpl = Model Nothing Nothing

newtype Resp = Resp {getResp :: Either String Success}

-- | An event records the model before and after a command along with the
-- command itself, and a mocked version of the response.
data Event r = Event
  { eventBefore   :: Model  r
  , eventCmd      :: At Cmd r
  , eventAfter    :: Model  r
  , eventMockResp :: Resp
  }

lockstep :: forall r.
            Model   r
         -> At Cmd  r
         -> At Resp r
         -> Event   r
lockstep model@Model {..} cmd (At resp) = Event
    { eventBefore   = model
    , eventCmd      = cmd
    , eventAfter    = model'
    , eventMockResp = mockResp
    }
  where
    (mockResp, model') = step model cmd

-- | Key property of the model is that we can go from real to mock responses
toMock :: Model r -> At t r -> t
toMock _ (At t) = t

step :: Model r -> At Cmd r -> (Resp, Model r)
step model@Model{..} (At cmd)  = case cmd of
    Migrate (migration, PersonTag) ->
        case persons of
            Just ls -> (Resp $ Left "table already exists..", model)
            Nothing -> (Resp $ Right $ Unit (), model{persons = Just []})
    Migrate (migration, CatTag) ->
        case cars of
            Just ls -> (Resp $ Left "table already exists..", model)
            Nothing -> (Resp $ Right $ Unit (), model{cars = Just []})
    Insert (TPerson person) ->
        case persons of
            Nothing -> (Resp $ Left "table doesn't exist..", model)
            Just ps -> if (canInsertP person ps)
                then (Resp $ Right $ Unit (), model{persons = Just (person : ps)})
                else (Resp $ Left "constraint error..", model)
    Insert (TCar car) ->
        case cars of
            Nothing -> (Resp $ Left "table doesn't exist..", model)
            Just cs -> if (canInsertC car cs)
                then case persons of -- foregn key check.
                    Nothing -> (Resp $ Left "foreign key constaint..", model)
                    Just ps -> if (carOwner car `elem` (PersonKey . personName <$> ps ))
                        then (Resp $ Right $ Unit (), model{cars = Just (car : cs)})
                        else (Resp $ Left "constraint error..", model)
                else (Resp $ Left "constraint error..", model)
    SelectList (SPerson (TableSelect fop sop)) -> undefined
    SelectList (SCar (TableSelect fop sop)) -> undefined

canInsertP :: Person -> [Person] -> Bool
canInsertP p ps = not $ personName p `elem` (personName <$> ps)

canInsertC :: Car -> [Car] -> Bool
canInsertC c cs = not (carCid c `elem` (carCid <$> cs))

mockImpl :: Model Symbolic -> At Cmd Symbolic -> GenSym (At Resp Symbolic)
mockImpl model cmdErr = At <$> return mockResp
    where
        (mockResp, _model') = step model cmdErr

shrinkerImpl :: Model Symbolic -> At Cmd Symbolic -> [At Cmd Symbolic]
shrinkerImpl _ _ = []

generatorImpl :: Model Symbolic -> Maybe (Gen (At Cmd Symbolic))
generatorImpl m@Model {..} = Just $ At <$> arbitrary

semanticsImpl ::  MonadIO m => Pool SqlBackend -> At Cmd Concrete -> m (At Resp Concrete)
semanticsImpl poolBackend cmd = do
    case unAt cmd of
        Migrate (migration, _tag) -> do
            ret <- liftIO $ try $ flip runSqlPersistMPool poolBackend $ do
                runMigration migration
            case ret of
                Right () -> return $ At $ Resp $ Right $ Unit ()
                Left (e :: SomeException) ->
                    return $ At $ Resp $ Left $ displayException e
        Insert te-> do
            ret <- liftIO $ try $ flip runSqlPersistMPool poolBackend $ retryOnBusy $
                case te of
                    TPerson person -> insert_ person
                    TCar car -> insert_ car
            case ret of
                Right () -> return $ At $ Resp $ Right $ Unit ()
                Left (e :: SomeException) ->
                    return $ At $ Resp $ Left $ displayException e
        SelectList ts -> do
            ret <- liftIO $ try $ flip runSqlPersistMPool poolBackend $ retryOnBusy $
                case ts of
                    SPerson (TableSelect fop sop) -> do
                        (pers :: [Entity Person]) <- selectList fop sop
                        return $ TPerson . entityVal <$> pers
                    SCar (TableSelect fop sop) -> do
                        (pers :: [Entity Car]) <- selectList fop sop
                        return $ TCar . entityVal <$> pers
            case ret of
                Right ls -> return $ At $ Resp $ Right $ List ls
                Left (e :: SomeException) ->
                    return $ At $ Resp $ Left $ displayException e


preconditionImpl :: Model Symbolic -> At Cmd Symbolic -> Logic
preconditionImpl _ _ = Top

postconditionImpl :: Model Concrete -> At Cmd Concrete -> At Resp Concrete -> Logic
postconditionImpl _ _ _ = Top

transitionImpl :: Model r -> At Cmd r -> At Resp r -> Model r
transitionImpl model cmd = eventAfter . lockstep model cmd


sm :: MonadIO m => Pool SqlBackend -> StateMachine Model (At Cmd) m (At Resp)
sm  poolBackend = StateMachine {
     initModel     = initModelImpl
   , transition    = transitionImpl
   , precondition  = preconditionImpl
   , postcondition = postconditionImpl
   , generator     = generatorImpl
   , shrinker      = shrinkerImpl
   , semantics     = semanticsImpl poolBackend
   , mock          = mockImpl
   , invariant     = Nothing
   }

-- instance CommandNames (At Cmd) where
--     cmdName (At cmd) = case cmd of
--         Migrate {} -> "Migrate"
--         Insert {} -> "Insert"
--         SelectList {} -> "SelectList"


run :: IO ()
run = do
    poolBackend <- runStdoutLoggingT $ createSqlitePool "testdb" 2
    flip runSqlPersistMPool poolBackend $ do
        runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Car)
        runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Person)
        -- insert_ $ Car 1 " black" (PersonKey "Kostas")
        -- (pers :: [Entity Person]) <- selectList [] []
        -- liftIO $ print pers
    print $ persistFieldDef CarCid
    thread0 poolBackend
--    print $ persistUniqueKeys Car
--     p0 <- asyncBound $ thread0 lock poolBackend
--     p1 <- asyncBound $ thread1 lock poolBackend
--     --thread0 lock poolBackend
--     -- traverse wait [p0, p1]
--     waitBoth p0 p1
--     flip runSqlPersistMPool poolBackend $ do
--         (pers :: [Entity Person]) <- selectList [] []
--         liftIO $ print $ length pers
--       close' poolBackend


thread0 ::
    Pool SqlBackend -> IO ()
thread0 backend = do
    x <- runStdoutLoggingT $ flip runSqlPool backend $ do
            insert $ Person ("Kostas") 42
            (pers :: [Entity Person]) <-
                selectList [] [LimitTo 2, OffsetBy 0, Desc PersonAge, Asc PersonAge]
            return pers
    print x

-- expected =
--     "SQLite3 returned ErrorConstraint while attempting to perform step: UNIQUE constraint failed: person.name"
-- expected' =
--     "SQLite3 returned ErrorConstraint while attempting to perform step: cannot start a transaction within a transaction"
-- expected'' =
--     "SQLite3 returned ErrorError while attempting to perform step: cannot start a transaction within a transaction"
-- 
-- 
-- thread1 ::
--     MVar () -> Pool SqlBackend -> IO ()
-- thread1 lock backend = forM_ [1..1000] $ \n -> do
--  --   x <- try $ flip runSqlPersistM backend $ do
-- --        insert_ $ Person "John Doe" n
--     x <-  try $ -- withMVar lock $ \_ ->
--         runNoLoggingT $ flip runSqlPool backend $ retryOnBusy $
--             insert_ $ Person (show n) 0
--     case x of
--         Right _ -> return ()
--         Left (e :: SomeException) ->
--             return ()
--             -- let actualError = displayException e
--             
--             -- in error $ "pid 1, trial " ++ show n ++ ": " ++ actualError
--     return ()
-- 