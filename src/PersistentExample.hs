{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds                  #-}
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
{-# LANGUAGE DeriveAnyClass #-}

module PersistentExample where

import           Control.Concurrent hiding (myThreadId)
import qualified Control.Concurrent (myThreadId)
import           Control.Concurrent.Async
import           Control.Concurrent.Async.Queue
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad
import           Control.Monad.Logger
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Functor.Classes
import           Data.Kind (Type)
import           Data.List hiding (insert)
import           Data.Maybe
import           Data.Pool
import           Data.Typeable
import           Database.Persist
import           Database.Persist.Sql.Util
import           Database.Persist.Sqlite
import           Database.Persist.TH
-- import           Database.Sqlite -- (SqliteException(..))
import           Database.Sqlite hiding (step)
import           Data.Set (fromList)
import           Data.TreeDiff (Expr(App))
import           GHC.Generics
import           System.Directory
import           System.IO
import           System.Posix.Thread hiding (Key)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.StateMachine hiding (exists)
import           Test.StateMachine.Types hiding (exists)
import qualified Test.StateMachine.Types.Rank2 as Rank2

import qualified Data.Bifunctor.TH as TH

import           Orphans
import           Schema

newtype At t r = At {unAt :: (t (PRef r) (CRef r))}
  deriving (Generic)

type PRef =  Reference (Key Person)
type CRef = Reference (Key Car)

-- deriving instance Show1 r => Show (Cmd r)
deriving instance Show1 r => Show (At Resp r)
deriving instance Show1 r => Show (At Cmd r)
deriving instance Eq1   r => Eq   (At Resp r)

data TableEntity = TPerson Person
                 | TCar Car
                 deriving (Eq, Show, Ord)

data TableTag = PersonTag | CatTag
        deriving Show

data TableSelectOpts record = TableSelect [Filter record] [SelectOpt record]
        deriving Show
data TableSelect = SPerson (TableSelectOpts Person) | SCar (TableSelectOpts Car)
        deriving Show

data TableUpdateOpts record = TableUpdate [Update record]
        deriving Show

data TableUpdate kp kc = UPerson kp (TableUpdateOpts Person) | UCars kc (TableUpdateOpts Car)
        deriving Show

data Cmd kp kh = -- (r :: Type -> Type) =
      Migrate (Opaque Migration, TableTag)
    | Insert TableEntity
    | SelectList TableSelect
    | UpdateC (TableUpdate kp kh)
    | Dummy
    deriving (Show, Generic1)

data Model (r :: Type -> Type) = Model
    {
      dbModel :: DBModel
    , knownPerson :: [(PRef r, Int)]
    , knownCars   :: [(CRef r, Int)]
    } deriving (Generic, Show)

data DBModel = DBModel {
            persons       :: Maybe [(Int, Person)]
          , nextPerson    :: Int
          , cars          :: Maybe [(Int, Car)]
          , nextCar       :: Int
          } deriving (Generic, Show)

initModelImpl :: Bool -> Model r
initModelImpl open = Model (DBModel (Just []) 0 (Just []) 0) [] []

-- this forces SQlite. Maybe we can use SomeException to make it backend agnostic.
newtype Resp kp kc = Resp {getResp :: Either SqliteException (Success kp kc)}
    deriving (Show, Generic1)

instance (Eq kp, Eq kc) => Eq (Resp kp kc) where
    (Resp (Left e1)) == (Resp (Left e2)) = seError e1 == seError e2
    (Resp (Right r1)) == (Resp (Right r2)) = r1 == r2
    _ == _ = False

getPers :: Resp kp kc -> [kp]
getPers (Resp (Right (InsertedPerson kp))) = [kp]
getPers _ = []

getCars :: Resp kp kc -> [kc]
getCars (Resp (Right (InsertedCar kc))) = [kc]
getCars _ = []

data Success kp kc =
      Unit ()
    | InsertedPerson kp -- (Reference (Key Person) r)
    | InsertedCar kc -- (Reference (Key Car) r)
    | List [TableEntity]
    deriving (Show, Eq, Generic1)

unitResp :: Resp kp kc
unitResp = Resp (Right (Unit ()))

-- instance Eq (Resp kp kc) where
--     Resp (Right r1) == Resp (Right r2) = r1 == r2
--     Resp (Left e1) == Resp (Left e2) = seError e1 == seError e2
--     _ == _ = False

-- | An event records the model before and after a command along with the
-- command itself, and a mocked version of the response.
data Event r = Event
  { eventBefore   :: Model  r
  , eventCmd      :: At Cmd r
  , eventAfter    :: Model  r
  , eventMockResp :: Resp   Int Int
  }

lockstep :: forall  r. (Show1 r, Ord1 r)
         => Model   r
         -> At Cmd  r
         -> At Resp r
         -> Event   r
lockstep model@Model {..} cmd resp = Event
    { eventBefore   = model
    , eventCmd      = cmd
    , eventAfter    = Model {
                        dbModel = dbModel'
                      , knownPerson = union' knownPerson newPerson
                      , knownCars = union' knownCars newCars
                    }
    , eventMockResp = mockResp
    }
  where
    (mockResp, dbModel') = step model cmd
    newPerson = zip (getPers $ unAt resp) (getPers mockResp)
    newCars = zip (getCars $ unAt resp) (getCars mockResp)

union' :: forall k v. (Eq k, Eq v, Show k, Show v)
              => [(k, v)] -- Mapping known to have duplicate keys
              -> [(k, v)] -- With potential duplicates
              -> [(k, v)]
union' acc []             = acc
union' acc ((k, v) : kvs) =
    case lookup k acc of
      Just v' | v /= v' -> error $ renderError v'
      _otherwise        -> union' ((k, v) : acc) kvs
  where
    renderError :: v -> String
    renderError v' = intercalate " " [
          "Key"
        , show k
        , "with two different values"
        , show v
        , "and"
        , show v'
        ]

-- | Key property of the model is that we can go from real to mock responses
toMock :: Eq1 r => Model r -> At Resp r -> Resp Int Int
toMock Model{..} (At r) =
   bimap (\p -> fromMaybe (error "could not find person ref") $ lookup p knownPerson)
         (\c -> fromMaybe (error "could not find car ref") $ lookup c knownCars) r

step :: Eq1 r
     => Model r
     -> At Cmd r
     -> (Resp Int Int, DBModel)
step model@Model{..} cmd =  runPure dbModel cmd

runPure :: DBModel -> At Cmd r -> (Resp Int Int, DBModel)
runPure dbModel@DBModel{..} cmd  = case unAt cmd of
    Migrate (migration, PersonTag) ->
        case persons of
            Just ls -> (unitResp, dbModel)
            Nothing -> (unitResp, dbModel{persons = Just []})
    Migrate (migration, CatTag) ->
        case cars of
            Just ls -> (unitResp, dbModel)
            Nothing -> (unitResp, dbModel{cars = Just []})
    Insert (TPerson person) ->
        case persons of
            Nothing -> (Resp $ Left $ SqliteException ErrorError "? " "table doesn't exist", dbModel)
            Just ps -> if (canInsertP person $ snd <$> ps)
                then (Resp $ Right $ InsertedPerson nextPerson, dbModel{persons = Just ((nextPerson, person) : ps), nextPerson = nextPerson + 1})
                else (Resp $ Left $ SqliteException ErrorConstraint "? "  "constraint error..", dbModel)
    Insert (TCar car) ->
        case cars of
            Nothing -> (Resp $ Left $ SqliteException ErrorError "? " "table doesn't exist..", dbModel)
            Just cs -> if (canInsertC car $ snd <$> cs)
                then case persons of -- foregn key check.
                    Nothing -> (Resp $ Left $ SqliteException ErrorError "? " "table doesn't exist..", dbModel)
                    Just ps -> if (carOwner car `elem` (PersonKey . personName . snd <$> ps ))
                        then (Resp $ Right $ InsertedCar nextCar, dbModel{cars = Just ((nextCar, car) : cs), nextCar = nextCar + 1})
                        else (Resp $ Left $ SqliteException ErrorConstraint "? "  "constraint error..", dbModel)
                else (Resp $ Left $ SqliteException ErrorConstraint "? "  "constraint error..", dbModel)
    UpdateC (UPerson kp (TableUpdate uu)) ->
        case persons of
            Nothing -> (Resp $ Left $ SqliteException ErrorError "? " "table doesn't exist", dbModel)
            Just _ -> (unitResp, dbModel) -- case uu of
--                [Update PersonName _ _] -> (Resp $ Left $ SqliteException ErrorConstraint "? " "constraint error", dbModel)
--                _ -> (unitResp, dbModel)
    UpdateC (UCars kp (TableUpdate uu)) ->
        case cars of
            Nothing -> (Resp $ Left $ SqliteException ErrorError "? " "table doesn't exist", dbModel)
            Just _ -> (unitResp, dbModel)
    SelectList (SPerson (TableSelect fop sop)) ->
        case persons of
            Nothing -> (Resp $ Left $ SqliteException ErrorError "? "  "table doesn't exist..", dbModel)
            Just ps ->
--                let ps' = foldr applyP ps fop
--                let ps'' = selectP sop ps'
                 (Resp $ Right $ List $ TPerson . snd <$> ps, dbModel)

    SelectList (SCar (TableSelect fop sop)) ->
        case cars of
            Nothing -> (Resp $ Left  $ SqliteException ErrorError "? " "table doesn't exist..", dbModel)
            Just cs ->
--                let cs' = foldr applyC cs fop
                 (Resp $ Right $ List $ TCar . snd <$> cs, dbModel)
    Dummy -> (unitResp, dbModel)

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
mockImpl model cmdErr = At <$> bitraverse (const genSym) (const genSym) mockResp
    where
        (mockResp, _model') = step model cmdErr

shrinkerImpl :: Model Symbolic -> At Cmd Symbolic -> [At Cmd Symbolic]
shrinkerImpl _ _ = []

generatorImpl :: Model Symbolic -> Maybe (Gen (At Cmd Symbolic))
generatorImpl m@Model {..} = Just $ genCmd m


-- semanticsImpl' ::  MonadIO m => Pool SqlBackend -> MVar () -> Cmd Concrete -> m (At Resp Concrete)
-- semanticsImpl' poolBackend _lock cmd = do
--     case cmd of
--         Migrate (migration, _tag) -> do
--             ret <- liftIO $ try $ runStdoutLoggingT $ retryOnBusy $ flip runSqlPool poolBackend $
--                 runMigration $ unOpaque migration
--             case ret of
--                 Right () -> return $ Resp $ Right $ Unit ()
--                 Left (e :: SqliteException) ->
--                     return $ Resp $ Left e
--         Insert te-> do
--             ret <- liftIO $ try $ runStdoutLoggingT $ retryOnBusy $ flip runSqlPool poolBackend $
--                 case te of
--                     TPerson person -> insert_ person
--                     TCar car -> insert_ car
--             case ret of
--                 Right () -> return $ Resp $ Right $ Unit ()
--                 Left (e :: SqliteException) ->
--                     return $ Resp $ Left e
--         SelectList ts -> do
--             ret <- liftIO $ try $ runStdoutLoggingT $ retryOnBusy $ flip runSqlPool poolBackend $
--                 case ts of
--                     SPerson (TableSelect fop sop) -> do
--                         (pers :: [Entity Person]) <- selectList fop sop
--                         return $ TPerson . entityVal <$> pers
--                     SCar (TableSelect fop sop) -> do
--                         (pers :: [Entity Car]) <- selectList fop sop
--                         return $ TCar . entityVal <$> pers
--             case ret of
--                 Right ls -> return $ Resp $ Right $ List ls
--                 Left (e :: SqliteException) ->
--                     return $ Resp $ Left e
--         Dummy -> return $ Resp $ Right $ Unit ()

semanticsImpl ::  MonadIO m => AsyncWithPool SqlBackend -> MVar () -> At Cmd Concrete -> m (At Resp Concrete)
semanticsImpl poolBackend _lock cmd = At . Resp <$>
    case unAt cmd of
        Migrate (migration, _tag) -> do
            ret <- liftIO $ try $ runStdoutLoggingT $ flip runSqlAsyncWrite poolBackend $
                runMigration $ unOpaque migration
            case ret of
                Right () -> return $ Right $ Unit ()
                Left (e :: SqliteException) ->
                    return $ Left e
        Insert (TPerson person) -> do
            ret <- liftIO $ try $ runStdoutLoggingT $ flip runSqlAsyncWrite poolBackend $
                    insert person
            case ret of
                Right key -> return $ Right $ InsertedPerson $ reference key
                Left (e :: SqliteException) ->
                    return  $ Left e
        Insert (TCar car) -> do
            ret <- liftIO $ try $ runStdoutLoggingT $ flip runSqlAsyncWrite poolBackend $
                    insert car
            case ret of
                Right key -> return $ Right $ InsertedCar $ reference key
                Left (e :: SqliteException) ->
                    return $ Left e
        UpdateC (UPerson kp (TableUpdate uu)) -> do
            ret <- liftIO $ try $ runStdoutLoggingT $ flip runSqlAsyncWrite poolBackend $
                    update (concrete kp) uu
            case ret of
                Right _ -> return $ Right $ Unit ()
                Left (e :: SqliteException) ->
                    return $ Left e
        UpdateC (UCars kc (TableUpdate uu)) -> do
            ret <- liftIO $ try $ runStdoutLoggingT $ flip runSqlAsyncWrite poolBackend $
                    update (concrete kc) uu
            case ret of
                Right _ -> return $ Right $ Unit ()
                Left (e :: SqliteException) ->
                    return $ Left e
        SelectList ts -> do
            ret <- liftIO $ try $ runStdoutLoggingT $ flip runSqlAsyncRead poolBackend $
                case ts of
                    SPerson (TableSelect fop sop) -> do
                        (pers :: [Entity Person]) <- selectList fop sop
                        return $ TPerson . entityVal <$> pers
                    SCar (TableSelect fop sop) -> do
                        (pers :: [Entity Car]) <- selectList fop sop
                        return $ TCar . entityVal <$> pers
            case ret of
                Right ls -> return $ Right $ List ls
                Left (e :: SqliteException) ->
                    return $ Left e
        Dummy -> return $ Right $ Unit ()

preconditionImpl :: Model Symbolic -> At Cmd Symbolic -> Logic
preconditionImpl _ _ = Top

-- | Key property of the model is that we can go from real to mock responses
-- toMock :: Model r -> At t r -> t
-- toMock _ At t) = t

equalResp' :: (Eq kp, Eq kc, Show kp, Show kc) => Resp kp kc -> Resp kp kc -> Logic
equalResp' (Resp (Right _)) (Resp (Right _)) = Top
equalResp' r1 r2 = r1 .== r2


postconditionImpl :: Model Concrete -> At Cmd Concrete -> At Resp Concrete -> Logic
postconditionImpl model cmd resp =
--    equalResp' (toMock (eventAfter ev) resp) (eventMockResp ev)
    equalResp' (toMock (eventAfter ev) resp) (eventMockResp ev)
  where
    ev = lockstep model cmd resp

    -- this only check if the ansers both succeed or both fail with the same error.


    --toMock :: Model Concrete -> At Resp Concrete -> Resp Int Int
    --toMock = undefined
    -- A stricter equality.
--    equalResp r1 r2 = case (cmd, r1, r2) of
--        (SelectList (SPerson (TableSelect _ sop)), Resp (Right (List l1)), Resp (Right (List l2))) | orders sop->
--            l1 .== l2
--        (SelectList (SCar (TableSelect _ sop)), Resp (Right (List l1)), Resp (Right (List l2))) | orders sop->
--            l1 .== l2
--        (_, Resp (Right (List l1)), Resp (Right (List l2))) ->
--            fromList l1 .== fromList l2
--        _ -> r1 .== r2
--    ev = lockstep model cmd resp
--    orders = any ordering
--    ordering (OffsetBy _) = False
--    ordering (LimitTo _) = False
--    ordering (Asc _) = True
--    ordering (Desc _) = True

transitionImpl :: (Show1 r, Ord1 r) => Model r -> At Cmd r -> At Resp r -> Model r
transitionImpl model cmd =  eventAfter . lockstep model cmd

-- Boilerplate

-- deriving instance Generic1          Cmd
-- deriving instance Rank2.Functor     Cmd
-- deriving instance Rank2.Foldable    Cmd
-- deriving instance Rank2.Traversable Cmd

    -- deriving instance Generic1          (At Resp)
--  deriving instance Rank2.Foldable    (At Resp)
-- deriving instance Rank2.Functor     (At Resp)
-- deriving instance Rank2.Traversable (At Resp)

deriving instance ToExpr DBModel
deriving instance ToExpr (Model Concrete)
deriving instance ToExpr Person
deriving instance ToExpr (Key Person)
deriving instance Generic (Key Person)
deriving instance ToExpr Car
deriving instance Generic Person
deriving instance Generic Car
deriving instance Generic (Key Car)
-- deriving instance (ToExpr (Key Car))
instance ToExpr (Key Car) where
  toExpr key = App (show key) []


-- instance Rank2.Foldable (At Resp) where
--   foldMap = \f (At x) -> bifoldMap (app f) (app f) x
--     where
--       app :: (r x -> m) -> Reference x r -> m
--       app f (Reference x) = f x
-- 
-- instance Rank2.Foldable (At Cmd) where
--   foldMap = \f (At x) -> bifoldMap (app f) (app f) x
--     where
--       app :: (r x -> m) -> Reference x r -> m
--       app f (Reference x) = f x


sm :: MonadIO m => AsyncWithPool SqlBackend -> MVar () -> StateMachine Model (At Cmd) m (At Resp)
sm  poolBackend lock = StateMachine {
     initModel     = initModelImpl True
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
        db <- liftIO $ runStdoutLoggingT $ createSqliteAsyncPool "test.db" 5
        liftIO $ flip runSqlAsyncWrite db $ do
            runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Person)
            runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Car)
        lock <- liftIO $ newMVar ()
        (hist, _model, res) <- runCommands (sm db lock)  cmds
        prettyCommands smUnused hist $ res === Ok

prop_parallel_sqlite :: Property
prop_parallel_sqlite =
    forAllParallelCommands smUnused $ \cmds -> monadicIO $ do
        liftIO $ do
            removePathForcibly "dbs"
            createDirectory "dbs"
--        poolBackend <- liftIO $ runStdoutLoggingT $ createSqlitePool "dbs/sqlite.db" 5
        qBackend <- liftIO $ runStdoutLoggingT $ createSqliteAsyncPool "dbs/sqlite.db" 5
        liftIO $ flip runSqlAsyncWrite qBackend $ do
            runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Person)
            runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Car)
        -- createSqliteAsyncQueue "dbs/sqlite.db"
        lock <- liftIO $ newMVar ()
        prettyParallelCommands cmds =<< runParallelCommandsNTimes 1 (sm qBackend lock) cmds
        liftIO $ closeSqlAsyncPool qBackend
--        liftIO $ close' poolBackend

run :: IO ()
run = do
    -- print $ isIdField $ CarId
    verboseCheck $ prop_sequential_sqlite

run' :: IO ()
run' = do
    removePathForcibly "dbs-test"
    createDirectory "dbs-test"
    poolBackend <- runStdoutLoggingT $ createSqlitePool "dbs-test/sqlite.db" 2
    runStdoutLoggingT $ flip runSqlPool poolBackend $ do
    -- backend <- runStdoutLoggingT $ createSqliteBackend "dbs-test/sqlite.db"
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


run'' :: IO ()
run'' = do
    removePathForcibly "dbs-test"
    createDirectory "dbs-test"
    conn <- open "dbs-test/sqlite.db"
    (backend :: SqlBackend) <- wrapConnection conn (\_ _ _ _ -> return ())
    flip runSqlPersistM backend $ do
        runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Person)
    p0 <- async $ thread0'' backend -- poolBackend
    p1 <- async $ thread0'' backend
    _ <- replicateM 10 $ async $ forM_ [1..] $ \n -> do
--        pid <- myThreadId
         writeFile "/dev/null" (show $ fib 35) -- >> print ("ok-" ++ (show n) ++ (show pid))
         return ()

    -- thread1'' backend -- poolBackend
    _ <- waitBoth p0 p1
    close' backend
    return ()

thread0'' :: SqlBackend -> IO ()
thread0'' backend = do
--    pid <- myThreadId
    haskellThreadId <- Control.Concurrent.myThreadId
    forM_ [1..10000] $ \n -> do
        print n
        pid <- myThreadId
        print $ show n ++ "-" ++ show pid ++ "-" ++ show haskellThreadId
        runStdoutLoggingT $ flip runSqlConn backend $ do
            insert_ $ Person ("kostas-" ++ show n) n


thread1'' :: SqlBackend -> IO ()
thread1'' backend = forM_ [1..1000] $ \n -> do
    runStdoutLoggingT $ flip runSqlConn backend $ do
        insert_ $ Person ("john-" ++ show n) n
        -- (pers :: [Entity Person]) <- selectList [] []
        -- return ()

thread0 ::
    Pool SqlBackend -> IO ()
thread0 backend = forM_ [1..1000] $ \n -> do
    runStdoutLoggingT $ flip runSqlPool backend $ retryOnBusy $ do
        insert_ $ Person ("kostas-" ++ show n) n
--            runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Person)

thread1 ::
    Pool SqlBackend -> IO ()
thread1 backend = forM_ [1..1000] $ \n -> do
    runStdoutLoggingT $ flip runSqlPool backend $ retryOnBusy $ do
        insert_ $ Person ("john-" ++ show n) n
        -- (pers :: [Entity Person]) <- selectList [] []
        -- liftIO $ print pers
        -- insert_ $ Person (show n) 0
--            runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Car)

-- runSimpleWrite :: IO ()
-- runSimpleWrite = do


runMutex :: IO ()
runMutex = do
    db <- open "sqlite3-mutex.db"
    mvar <- newMVar False
    a <- async $ mutexThread 0 db mvar
    b <- asyncBound $ mutexThread 1 db mvar
    _ <- replicateM 2 $ async $ forM_ [1..] $ \n -> do
        -- pid <- myThreadId
        writeFile "/dev/null" (show $ fib 35) -- >>  -- print ("ok-" ++ (show n) ++ (show pid))
--    _ <- wait a
    _ <- waitBoth a b
    return ()

mutexThread :: Int -> Connection -> MVar Bool -> IO ()
mutexThread n db mvar = do
  haskellThreadId <- Control.Concurrent.myThreadId
  forM_ [1..1000] $ \n -> do
    _ <- mutex_enter db
    -- critical area.
--    pid <- myThreadId
--    print ("first action-" ++ (show n) ++ "-" ++ (show haskellThreadId) ++ "-" ++ (show pid))
    modifyMVar_ mvar $ \x -> case x of
        True -> throwIO $ SyncError $ "Thread " ++ show n ++ " expected False on trial " ++ show n
        False -> return True
    print $ fib 38
--    pid <- myThreadId
--    print ("second action-" ++ (show n) ++ "-" ++ (show haskellThreadId) ++ "-" ++ (show pid))
    modifyMVar_ mvar $ \x -> case x of
        False -> throwIO $ SyncError $ "Thread " ++ show n ++ " expected True on trial " ++ show n
        True -> return False
--    print ("leaving-" ++ (show n) ++ "-" ++ (show haskellThreadId) ++ "-" ++ (show pid))
    mutex_leave db
    print n

data SyncError = SyncError String deriving (Show, Typeable, Exception)


fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


-- Generators

migrations :: [(Migration, TableTag)]
migrations = [
      (migrate entityDefs $ entityDef (Nothing :: Maybe Car), CatTag)
    , (migrate entityDefs $ entityDef (Nothing :: Maybe Person), PersonTag)
    ]

names :: [String]
names = ["John", "Stevan", "Kostas", "Curry", "Robert"]

colors :: [String]
colors = ["black", "blue", "red", "yellow"]

genColor :: Gen (Maybe String)
genColor = (elements $ Nothing : (Just <$> colors))


instance Arbitrary Person where
    arbitrary = Person <$> elements names
                       <*> suchThat arbitrary (>0)

instance Arbitrary Car where
    arbitrary = Car <$> arbitrary
                    <*> genColor
                    <*> (PersonKey <$> elements names)

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

instance Arbitrary PersistUpdate where
    arbitrary = elements [Assign, Add, Subtract, Multiply] -- Divide is sneaky

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

instance Arbitrary (Update Person) where
    arbitrary = do
        uu <- arbitrary
--        let listOk = allowsList ff
        let genName = do
                names :: String <- arbitrary
                return Update {
                      updateField = PersonName
                    , updateValue = names
                    , updateUpdate = uu
                }
        let genAge = do
                age <- arbitrary
                return Update {
                      updateField = PersonAge
                    , updateValue = age
                    , updateUpdate = uu
                }
        frequency [(0,genName), (1,genAge)]

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


instance Arbitrary (Update Car) where
    arbitrary = do
      uu <- arbitrary
--      let genId = do
--            n <- arbitrary
--            return Update {
--                  updateField = CarId
--                , updateValue = n
--                , updateUpdate = uu
--                }
      let genCid = do
            n <- arbitrary
            return Update {
                  updateField = CarCid
                , updateValue = n
                , updateUpdate = uu
                }
      let genColor' = do
            color <- arbitrary
            return Update {
                  updateField = CarColor
                , updateValue = color
                , updateUpdate = uu
                }
--      let genOwner = do
--            name <- arbitrary
--            return Update {
--                  updateField = CarOwner
--                , updateValue = name
--                , updateUpdate = uu
--                }
      oneof [genCid, genColor'] -- , genOwner]


instance Arbitrary TableEntity where
    arbitrary = do
        frequency [ (1, TPerson <$> arbitrary)
                , (1, TCar <$> arbitrary)
                ]

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

genUpdate :: Model Symbolic -> Maybe (Gen (TableUpdate (PRef Symbolic) (CRef Symbolic)))
genUpdate m@Model{..} = do
    let genPerson = do
            kp <- fst <$> elements knownPerson
            up <- arbitrary
            return $ UPerson kp $ TableUpdate [up]

    let genCar = do
            kc <- fst <$> elements knownCars
            uc <- arbitrary
            return $ UCars kc $ TableUpdate [uc]
    let np = if null knownPerson then 0 else 1
    let nc = if null knownCars then 0 else 1
    if np + nc == 0 then Nothing else Just $ frequency [(np, genPerson), (nc, genCar)]

genCmd :: Model Symbolic -> Gen (At Cmd Symbolic)
genCmd m@Model{..} = At <$>
        frequency [ (0, Migrate . (\(a,b) -> (Opaque a,b)) <$> elements migrations)
                  , (3, Insert <$> arbitrary)
                  , (3, SelectList <$> arbitrary)
                  , (if isJust (genUpdate m) then 1 else 0, UpdateC  <$> (fromJust $ genUpdate m))
                  , (1, return Dummy)
                  ]

instance Bifoldable t => Rank2.Foldable (At t) where
  foldMap = \f (At x) -> bifoldMap (app f) (app f) x
    where
      app :: (r x -> m) -> Reference x r -> m
      app f (Reference x) = f x

instance Bifunctor t => Rank2.Functor (At t) where
  fmap = \f (At x) -> At (bimap (app f) (app f) x)
    where
      app :: (r x -> r' x) -> Reference x r -> Reference x r'
      app f (Reference x) = Reference (f x)

instance Bitraversable t => Rank2.Traversable (At t) where
  traverse = \f (At x) -> At <$> bitraverse (app f) (app f) x
    where
      app :: Functor f
          => (r x -> f (r' x)) -> Reference x r -> f (Reference x r')
      app f (Reference x) = Reference <$> f x


TH.deriveBifunctor     ''Success
TH.deriveBifoldable    ''Success
TH.deriveBitraversable ''Success

TH.deriveBifunctor     ''Resp
TH.deriveBitraversable ''Resp
TH.deriveBifoldable    ''Resp

TH.deriveBifunctor     ''TableUpdate
TH.deriveBifoldable    ''TableUpdate
TH.deriveBitraversable ''TableUpdate

TH.deriveBifunctor     ''Cmd
TH.deriveBifoldable    ''Cmd
TH.deriveBitraversable ''Cmd
