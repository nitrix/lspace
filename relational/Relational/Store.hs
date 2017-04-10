{-# LANGUAGE RankNTypes #-}

module Relational.Store
    ( Store(..)
    ) where

import Control.Monad.State
import Relational.Types

-- TODO: Maybe each store should do their own caching?
-- TODO: MEaning we need a Cache module and get rid of the storeCacheable

-- TODO: This is great, because otherwise we'll need a notify system or something for a ViaNetwork to know when updates occur.

class Store via where
    -- CRUD store
    storeCreate    :: forall a. Related a => a -> StateT via IO Integer
    storeRead      :: forall a. Related a => Integer -> StateT via IO a
    storeUpdate    :: forall a. Related a => Integer -> a -> StateT via IO ()
    storeDelete    :: Integer -> StateT via IO ()
    storeInit      :: StateT via IO ()
    storeQuit      :: StateT via IO ()
    storeCacheable :: forall p. p via -> Bool -- TODO: Proxy !?