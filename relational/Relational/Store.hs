{-# LANGUAGE RankNTypes #-}

module Relational.Store
    ( Store(..)
    ) where

import Control.Monad.State (StateT, lift)
import Data.Dynamic
import Data.Label.Monadic
import qualified Data.Map as M

import Relational.Types

-- TODO: Maybe each store should be its own module?

class Store via where
    -- CRUD store
    storeCreate    :: forall a. Related a => a -> StateT via IO Integer
    storeRead      :: forall a. Related a => Integer -> StateT via IO a
    storeUpdate    :: forall a. Related a => Integer -> a -> StateT via IO ()
    storeDelete    :: Integer -> StateT via IO ()
    -- Whether or not it makes sense for a given store to use memory cache
    storeCacheable :: forall p. p via -> Bool -- TODO: Proxy !?