module Relational.Core
    ( createRelation
    , readRelation
    , writeRelation
    ) where

import Control.Monad.State
import Data.Dynamic
import Data.IORef
import System.Mem.Weak

import Relational.Store

type RelationId = Integer
data Relation a = Relation (RelationId, IORef (Maybe (Weak (IORef a))))
newtype Relational s a = Relational { unwrapRelational :: StateT s IO a }

createRelation :: (Store s, Typeable a) => a -> Relational s (Relation a)
createRelation = undefined

readRelation :: (Store s, Typeable a) => Relation a -> Relational s a
readRelation = undefined

writeRelation :: (Store s, Typeable a) => Relation a -> a -> Relational s ()
writeRelation = undefined
