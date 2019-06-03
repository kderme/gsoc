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

module Schema where

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
-- import           Database.Persist.Class.PersistEntity
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.StateMachine
import           Test.StateMachine.Types
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Data.Functor.Classes

share [mkPersist sqlSettings, mkSave "entityDefs"] [persistLowerCase|
Person
    name String
    age Int
    Primary name
    deriving Show
    deriving Eq
    deriving Ord

Car
    cid Int
    color String Maybe
    owner PersonId -- foreign key
    UniqueCid cid
    deriving Show
    deriving Eq
    deriving Ord
|]
