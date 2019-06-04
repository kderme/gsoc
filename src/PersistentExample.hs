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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass      #-}

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
import           Data.Set (fromList)
import           Database.Sqlite hiding (step)
import           Control.Monad.Logger
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.StateMachine hiding (exists)
import           Test.StateMachine.Types hiding (exists)
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Data.Functor.Classes
import           System.Directory

import Database.Persist.Sql.Util
import           Database.Sqlite (SqliteException(..))


import Schema

names :: [String]
names = ["John", "Stevan", "Kostas", "Curry", "Robert"]

colors :: [String]
colors = ["black", "blue", "red", "yellow"]

-- Boilerplate

deriving instance Generic1    (At Cmd)
deriving instance Rank2.Functor     (At Cmd)
deriving instance Rank2.Foldable    (At Cmd)
deriving instance Rank2.Traversable (At Cmd)

deriving instance Rank2.Foldable (At Resp)
deriving instance Generic1 (At Resp)
deriving instance ToExpr (Model Concrete)
deriving instance ToExpr Person
deriving instance ToExpr (Key Person)
deriving instance Generic (Key Person)
deriving instance ToExpr Car
deriving instance Generic Person
deriving instance Generic Car

instance Arbitrary Person where
    arbitrary = Person <$> elements names
                       <*> suchThat arbitrary (>0)

instance Arbitrary Car where
    arbitrary = Car <$> arbitrary
                    <*> genColor
                    <*> (PersonKey <$> elements names)

genColor :: Gen (Maybe String)
genColor = (elements $ Nothing : (Just <$> colors))

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
      let listOk = allowsList ff
      let genName = do
            names <- genFilterVal listOk (elements names)
            return Filter {
                  filterField = PersonName
                , filterValue = names
                , filterFilter = ff
                }
      let genAge = do
            ages <- genFilterVal listOk $ suchThat arbitrary (>=0)
            return Filter {
                  filterField = PersonAge
                , filterValue = ages
                , filterFilter = ff
                }
      oneof [genName, genAge]

allowsList :: PersistFilter -> Bool
allowsList Eq = False
allowsList Ne = False
allowsList Gt = False
allowsList Lt = False
allowsList Ge = False
allowsList Le = False
allowsList In = True
allowsList NotIn  = True
allowsList (BackendSpecificFilter _) = error "not allown"

genFilterVal :: Bool -> Gen typ -> Gen (FilterValue typ)
genFilterVal allowsList gen = frequency [(1, gen1), (if allowsList then 1 else 0, gen2)]
    where
        gen1 = do
            tp <- gen
            return $ FilterValue tp
        gen2 = do
            tp <- listOf gen
            return $ FilterValues tp

instance Arbitrary (Filter Car) where
    arbitrary = do
      ff <- arbitrary
      let listOk = allowsList ff
      let genId = do
            n <- genFilterVal listOk (CarKey <$> (return $ SqlBackendKey 1))
            return Filter {
                  filterField = CarId
                , filterValue = n
                , filterFilter = ff
                }
      let genCid = do
            n <- genFilterVal listOk arbitrary
            return Filter {
                  filterField = CarCid
                , filterValue = n
                , filterFilter = ff
                }
      let genColor' = do
            color <- genFilterVal listOk $ genColor
            return Filter {
                  filterField = CarColor
                , filterValue = color
                , filterFilter = ff
                }
      let genOwner = do
            name <- genFilterVal listOk $ PersonKey <$> elements names
            return Filter {
                  filterField = CarOwner
                , filterValue = name
                , filterFilter = ff
                }
      oneof [genId, genCid, genColor', genOwner]

data TableEntity = TPerson Person
                 | TCar Car
                 deriving (Eq, Show, Ord)

data TableTag = PersonTag | CatTag
        deriving Show

instance Arbitrary TableEntity where
    arbitrary = do
        frequency [ (1, TPerson <$> arbitrary)
                  , (1, TCar <$> arbitrary)
                  ]

migrations :: [(Migration, TableTag)]
migrations = [
      (migrate entityDefs $ entityDef (Nothing :: Maybe Car), CatTag)
    , (migrate entityDefs $ entityDef (Nothing :: Maybe Person), PersonTag)
    ]

data TableSelectOpts record = TableSelect [Filter record] [SelectOpt record]
        deriving Show
data TableSelect = SPerson (TableSelectOpts Person) | SCar (TableSelectOpts Car)
        deriving Show

instance (Arbitrary (Filter record), Arbitrary (SelectOpt record))
    => Arbitrary (TableSelectOpts record) where
        arbitrary = do
            nf <- choose (0,3)
            fop <- replicateM nf arbitrary
            ns <- choose (0,1)
            sop <- replicateM ns arbitrary
            return $ TableSelect fop sop

instance Arbitrary TableSelect where
    arbitrary = frequency [
                  (1, SPerson <$> arbitrary)
                , (1, SCar <$> arbitrary)
                ]

data Cmd =
      Migrate (Opaque Migration, TableTag)
    | Insert TableEntity
    | SelectList TableSelect
    deriving (Show)

instance Arbitrary Cmd where
    arbitrary = do
        frequency [ (1, Migrate . (\(a,b) -> (Opaque a,b)) <$> elements migrations)
                  , (3, Insert <$> arbitrary)
                  , (3, SelectList <$> arbitrary)
                  ]

newtype At t (r :: Type -> Type) = At {unAt :: t}
    deriving (Generic, Show)

-- | Alias for 'At'
type (:@) t r = At t r

data Success =
      Unit ()
    | List [TableEntity]
    deriving (Show, Eq)

data Model (r :: Type -> Type) = Model
    { persons :: Maybe [Person]
    , cars    :: Maybe [Car]
    } deriving (Generic, Show)

initModelImpl = Model Nothing Nothing

-- this forces SQlite. Maybe we can use SomeException to make it backend agnostic.
newtype Resp = Resp {getResp :: Either SqliteException Success}
    deriving Show

instance Eq Resp where
    Resp (Right r1) == Resp (Right r2) = r1 == r2
    Resp (Left e1) == Resp (Left e2) = seError e1 == seError e2
    _ == _ = False

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
            Just ls -> (Resp $ Right $ Unit (), model)
            Nothing -> (Resp $ Right $ Unit (), model{persons = Just []})
    Migrate (migration, CatTag) ->
        case cars of
            Just ls -> (Resp $ Right $ Unit (), model)
            Nothing -> (Resp $ Right $ Unit (), model{cars = Just []})
    Insert (TPerson person) ->
        case persons of
            Nothing -> (Resp $ Left $ SqliteException ErrorError "?" "table doesn't exist", model)
            Just ps -> if (canInsertP person ps)
                then (Resp $ Right $ Unit (), model{persons = Just (person : ps)})
                else (Resp $ Left $ SqliteException ErrorConstraint "?"  "constraint error..", model)
    Insert (TCar car) ->
        case cars of
            Nothing -> (Resp $ Left $ SqliteException ErrorError "?" "table doesn't exist..", model)
            Just cs -> if (canInsertC car cs)
                then case persons of -- foregn key check.
                    Nothing -> (Resp $ Left $ SqliteException ErrorError "?" "table doesn't exist..", model)
                    Just ps -> if (carOwner car `elem` (PersonKey . personName <$> ps ))
                        then (Resp $ Right $ Unit (), model{cars = Just (car : cs)})
                        else (Resp $ Left $ SqliteException ErrorConstraint "?"  "constraint error..", model)
                else (Resp $ Left $ SqliteException ErrorConstraint "?"  "constraint error..", model)
    SelectList (SPerson (TableSelect fop sop)) ->
        case persons of
            Nothing -> (Resp $ Left $ SqliteException ErrorError "?"  "table doesn't exist..", model)
            Just ps ->
--                let ps' = foldr applyP ps fop
--                let ps'' = selectP sop ps'
                 (Resp $ Right $ List $ TPerson <$> ps, model)

    SelectList (SCar (TableSelect fop sop)) ->
        case cars of
            Nothing -> (Resp $ Left  $ SqliteException ErrorError "?" "table doesn't exist..", model)
            Just cs ->
--                let cs' = foldr applyC cs fop
                 (Resp $ Right $ List $ TCar <$> cs, model)


-- selectP :: [SelectOpt Person] -> [Person] -> [Person]
-- selectP sops ps =
--     case findOrd sopsof of
--         Nothing

applyP :: Filter Person -> [Person] -> [Person]
applyP f ps = case f of
    Filter PersonName (FilterValue name) ff ->
        snd <$> filterPF name ((\p -> (personName p, p)) <$> ps) (filterFilter f)
    Filter PersonAge (FilterValue age) ff ->
        snd <$> filterPF age ((\p -> (personAge p, p)) <$> ps) (filterFilter f)
    _ -> undefined

applyC :: Filter Car -> [Car] -> [Car]
applyC f cs = case f of
    Filter CarCid (FilterValue cid) ff ->
        snd <$> filterPF cid ((\c -> (carCid c, c)) <$> cs) (filterFilter f)
    Filter CarColor (FilterValue color) ff ->
        snd <$> filterPF color ((\c -> (carColor c, c)) <$> cs) (filterFilter f)
    Filter CarOwner (FilterValue (owner)) ff ->
        snd <$> filterPF owner ((\c -> (carOwner c, c)) <$> cs) (filterFilter f)
    _ -> undefined


filterPF ::Ord a => Eq a => a -> [(a, b)] -> PersistFilter -> [(a, b)]
filterPF a abs pf = filter (\t -> op (fst t) a) abs
    where
        op = case pf of
            Eq -> (==)
            Ne -> (/=)
            Gt -> (>)
            Lt -> (<)
            Ge -> (>=)
            Le -> (<=)

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

semanticsImpl ::  MonadIO m => Pool SqlBackend -> MVar () -> At Cmd Concrete -> m (At Resp Concrete)
semanticsImpl poolBackend _lock cmd = do
    case unAt cmd of
        Migrate (migration, _tag) -> do
            ret <- liftIO $ try $ runStdoutLoggingT $ retryOnBusy $ flip runSqlPool poolBackend $
                runMigration $ unOpaque migration
            case ret of
                Right () -> return $ At $ Resp $ Right $ Unit ()
                Left (e :: SqliteException) ->
                    return $ At $ Resp $ Left e
        Insert te-> do
            ret <- liftIO $ try $ runStdoutLoggingT $ retryOnBusy $ flip runSqlPool poolBackend $
                case te of
                    TPerson person -> insert_ person
                    TCar car -> insert_ car
            case ret of
                Right () -> return $ At $ Resp $ Right $ Unit ()
                Left (e :: SqliteException) ->
                    return $ At $ Resp $ Left e
        SelectList ts -> do
            ret <- liftIO $ try $ runStdoutLoggingT $ retryOnBusy $ flip runSqlPool poolBackend $
                case ts of
                    SPerson (TableSelect fop sop) -> do
                        (pers :: [Entity Person]) <- selectList fop sop
                        return $ TPerson . entityVal <$> pers
                    SCar (TableSelect fop sop) -> do
                        (pers :: [Entity Car]) <- selectList fop sop
                        return $ TCar . entityVal <$> pers
            case ret of
                Right ls -> return $ At $ Resp $ Right $ List ls
                Left (e :: SqliteException) ->
                    return $ At $ Resp $ Left e


preconditionImpl :: Model Symbolic -> At Cmd Symbolic -> Logic
preconditionImpl _ _ = Top

postconditionImpl :: Model Concrete -> At Cmd Concrete -> At Resp Concrete -> Logic
postconditionImpl model cmd resp =
    equalResp' (toMock (eventAfter ev) resp) (eventMockResp ev)
  where
    -- this only check if the ansers both succed or both fail.
    equalResp' (Resp (Right _)) (Resp (Right _)) = Top
    equalResp' (Resp (Left _)) (Resp (Left _)) = Top
    equalResp' _ _ = Bot
    -- A stricter equality.
    equalResp r1 r2 = case (unAt cmd, r1, r2) of
        (SelectList (SPerson (TableSelect _ sop)), Resp (Right (List l1)), Resp (Right (List l2))) | orders sop->
            l1 .== l2
        (SelectList (SCar (TableSelect _ sop)), Resp (Right (List l1)), Resp (Right (List l2))) | orders sop->
            l1 .== l2
        (_, Resp (Right (List l1)), Resp (Right (List l2))) ->
            fromList l1 .== fromList l2
        _ -> r1 .== r2
    ev = lockstep model cmd resp
    orders = any ordering
    ordering (OffsetBy _) = False
    ordering (LimitTo _) = False
    ordering (Asc _) = True
    ordering (Desc _) = True


transitionImpl :: Model r -> At Cmd r -> At Resp r -> Model r
transitionImpl model cmd = eventAfter . lockstep model cmd


sm :: MonadIO m => Pool SqlBackend -> MVar () -> StateMachine Model (At Cmd) m (At Resp)
sm  poolBackend lock = StateMachine {
     initModel     = initModelImpl
   , transition    = transitionImpl
   , precondition  = preconditionImpl
   , postcondition = postconditionImpl
   , generator     = generatorImpl
   , shrinker      = shrinkerImpl
   , semantics     = semanticsImpl poolBackend lock
   , mock          = mockImpl
   , invariant     = Nothing
   }

smUnused :: StateMachine Model (At Cmd) IO (At Resp)
smUnused = sm undefined undefined

prop_sequential_sqlite :: Property
prop_sequential_sqlite =
    forAllCommands smUnused Nothing $ \cmds -> monadicIO $ do
        liftIO $ removePathForcibly "test.db"
        poolBackend <- liftIO $ runStdoutLoggingT $ createSqlitePool "test.db" 5
        lock <- liftIO $ newMVar ()
        (hist, _model, res) <- runCommands (sm poolBackend lock)  cmds
        prettyCommands smUnused hist $ res === Ok

prop_parallel_sqlite ::  Property
prop_parallel_sqlite =
    forAllGeneralParallelCommands smUnused 4 $ \cmds -> monadicIO $ do
        liftIO $ do
            removePathForcibly "dbs"
            createDirectory "dbs"
        poolBackend <- liftIO $ runStdoutLoggingT $ createSqlitePool "dbs/sqlite.db" 5
        lock <- liftIO $ newMVar ()
        prettyGeneralParallelCommands cmds =<< runGeneralParallelCommandsNTimes 1 (sm poolBackend lock) cmds
        liftIO $ destroyAllResources poolBackend
--        liftIO $ close' poolBackend

run :: IO ()
run = do
    -- print $ isIdField $ CarId
    verboseCheck $ prop_parallel_sqlite

connStr = "host=localhost dbname=test user=test password=test port=5432"

run' :: IO ()
run' = do
    removePathForcibly "dbs-test"
    createDirectory "dbs-test"
    poolBackend <- runStdoutLoggingT $ createSqlitePool "dbs-test/sqlite.db" 2
    runStdoutLoggingT $ flip runSqlPool poolBackend $ do
    --     runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Person)
         runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Person)
         runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Car)
    --     --insert_ $ Car 1 " black" (PersonKey "Kostas")
    --     insert_ $ Person "kostas" 5
    --     insert_ $ Person "John" 10
    --     (pers :: [Entity Person]) <- selectList [Filter PersonAge (FilterValue 1) Ne] []
        -- (pers :: [Entity Person]) <- selectList []
--        (pers :: [Entity Person]) <- selectList [] [LimitTo 0, OffsetBy 1]
       -- liftIO $ print pers
--    thread0 poolBackend
--    print $ persistUniqueKeys Car
    p0 <- async $ thread0 poolBackend
    p1 <- async $ thread1 poolBackend
--     --thread0 lock poolBackend
--     -- traverse wait [p0, p1]
    _ <- waitBoth p0 p1
    destroyAllResources poolBackend
    return ()
--    flip runSqlPersistMPool poolBackend $ do
--        (pers :: [Entity Person]) <- selectList [] []
--        liftIO $ print $ length pers
--    close' poolBackend


thread0 ::
    Pool SqlBackend -> IO ()
thread0 backend = forM_ [1..1000] $ \n -> do
    runStdoutLoggingT $ flip runSqlPool backend $ retryOnBusy $ do
        insert_ $ Person ("kostas" ++ show n) n
--            runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Person)

thread1 ::
    Pool SqlBackend -> IO ()
thread1 backend = forM_ [1..1000] $ \n -> do
    runStdoutLoggingT $ flip runSqlPool backend $ retryOnBusy $ do
        insert_ $ Person (show n) 0
--            runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Car)


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