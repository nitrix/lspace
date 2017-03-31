{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Relational.Store
    ( Store(..)
    , viaDisk
    , viaMemory
    ) where

import Control.Monad.State (StateT)
import Data.Dynamic
import Data.Label (mkLabel)
import Data.Label.Monadic
import qualified Data.Map as M

newtype ViaMemory = ViaMemory { _memoryMapping :: M.Map Integer Dynamic }
newtype ViaDisk = ViaDisk { _diskNamespace :: String }
mkLabel ''ViaMemory

class Store v where
    storeRead  :: forall a. Typeable a => Integer -> StateT v IO a
    storeWrite :: forall a. Typeable a => Integer -> a -> StateT v IO ()

instance Store ViaMemory where
    storeRead relationId = do
        m <- gets memoryMapping
        case M.lookup relationId m of
            Just found -> do
                case fromDynamic found of
                    Just value -> return value
                    Nothing    -> error "The value in memory has the wrong representation for the relation requested"
            Nothing    -> error "The relation requested was not found in memory"
    storeWrite relationId value = do
        modify memoryMapping (M.insert relationId (toDyn value))

-- TODO: Read and write from "data/" <> namespace <> "/" <> relationId <> ".json"

viaMemory :: ViaMemory
viaMemory = ViaMemory M.empty

viaDisk :: String -> ViaDisk
viaDisk namespace = ViaDisk namespace
