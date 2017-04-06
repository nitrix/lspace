module Relational.Store.Memory where

import Data.Dynamic
import Data.Label.Monadic (gets, modify)
import qualified Data.Map as M

import Relational.Store
import Relational.Types

instance Store ViaMemory where
    storeRead relationId = do
        mm <- gets memoryMapping
        case M.lookup relationId mm of
            Just found -> do
                case fromDynamic found of
                    Just value -> return value
                    Nothing    -> error "The value in memory has the wrong representation for the relation requested"
            Nothing -> error "The relation requested was not found in memory"
    storeUpdate relationId value = do
        modify memoryMapping (M.insert relationId (toDyn value))
    storeCacheable = const False

viaMemory :: ViaMemory
viaMemory = ViaMemory M.empty 0