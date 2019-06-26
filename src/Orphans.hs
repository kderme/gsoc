module Orphans where

import           Database.Persist
import           Database.Persist.Sql.Util
import           Database.Persist.Sqlite
import           Database.Persist.TH

import Schema

instance Show (Filter record) where
    show (Filter _ fv ff) = show ff ++ " " ++ show fv
    show (FilterOr ls) = show ls
    show _ = "Can't show Filter"

instance (PersistField typ) => Show (FilterValue typ) where
    show (FilterValue typ) = show $ toPersistValue typ
    show (FilterValues typs) = show $ toPersistValue <$> typs
    show (UnsafeValue _) = "UnsafeValue"

instance PersistEntity record => Show  (SelectOpt record) where
    show (Asc pfd) = "Asc " ++ (show $ unHaskellName $ fieldHaskell $ persistFieldDef pfd)
    show (Desc pfd) = "Desc " ++ (show $ fieldHaskell $ persistFieldDef pfd)
    show (OffsetBy n) = "OffsetBy " ++ show n
    show (LimitTo n) = "LimitTo " ++ show n

instance Show (Update record) where
    show (Update _ fv u) = show u ++ " " ++ (show $ toPersistValue fv)
    show (BackendUpdate _) = "BackendUpdate ?"
