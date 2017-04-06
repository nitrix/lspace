{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module Relational.Core
    ( newRelation
    , readRelation
    , writeRelation
    , runRelational
    ) where

import Control.Monad.State
import Data.Aeson
import Data.Dynamic

import Relational.Store

type Related a = (Typeable a, ToJSON a, FromJSON a)

type RelationId = Integer
data Relation a = Relation RelationId
newtype Relational s a = Relational { unwrapRelational :: StateT s IO a } deriving (Functor, Applicative, Monad, MonadState s)
-- TODO: Going to need a special type in there instead of `s` that contains the autoincrement and the cache (using hashtables linear?)
-- Maybe a new monad transformer? I don't know.

runRelational :: s -> Relational s () -> IO ()
runRelational store relational = do
    evalStateT (unwrapRelational relational) store
    -- TODO: write all elements in the cache

newRelation :: (Store s, Related a) => a -> Relational s (Relation a)
newRelation _ = do
    -- TODO: Crap this is going to change the `s` in the `Relational s a` for a custom type that has the autoincrement.
    return (Relation 100)

readRelation :: (Store s, Related a) => Relation a -> Relational s a
readRelation (Relation relationId) = do
    -- TODO: Caching
    
    -- Otherwise, we read from the store
    -- TODO: Add to cache
    store <- get
    (result, newStore) <- relationLiftIO $ runStateT (storeRead relationId) store
    put newStore
    return result

writeRelation :: (Store s, Related a) => Relation a -> a -> Relational s ()
writeRelation (Relation relationId) val = do
    -- TODO: Update cache
    
    -- TODO: Remove this, we don't need to write to disk on each write. I'm just testing.
    store <- get
    (result, newStore) <- relationLiftIO $ runStateT (storeWrite relationId val) store
    put newStore
    return result

relationLiftIO :: IO a -> Relational s a
relationLiftIO = Relational . liftIO