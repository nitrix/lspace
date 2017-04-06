{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Relational.Store
    ( Store(..)
    , viaDisk
    , viaMemory
    ) where

import Control.Monad.State (StateT, lift)
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Dynamic
import Data.Label (mkLabel)
import Data.Label.Monadic
import qualified Data.Map as M
import Data.Monoid

newtype ViaMemory = ViaMemory { _memoryMapping :: M.Map Integer Dynamic }
newtype ViaDisk = ViaDisk { _diskNamespace :: String }
mkLabel ''ViaMemory
mkLabel ''ViaDisk

class Store v where
    storeRead  :: forall a. (Typeable a, FromJSON a) => Integer -> StateT v IO a
    storeWrite :: forall a. (Typeable a, ToJSON a)  => Integer -> a -> StateT v IO ()

instance Store ViaMemory where
    storeRead relationId = do
        mm <- gets memoryMapping
        case M.lookup relationId mm of
            Just found -> do
                case fromDynamic found of
                    Just value -> return value
                    Nothing    -> error "The value in memory has the wrong representation for the relation requested"
            Nothing -> error "The relation requested was not found in memory"

    storeWrite relationId value = do
        modify memoryMapping (M.insert relationId (toDyn value))

instance Store ViaDisk where
    storeRead relationId = do
        name   <- gets diskNamespace
        result <- lift $ LB.readFile ("data/" <> name <> "/" <> show relationId <> ".json")
        -- TODO: catch not found IO exception
        case decode result of
            Just r  -> return r
            Nothing -> error "The relation requested is a malformed JSON"

    storeWrite relationId value = do
        name <- gets diskNamespace
        lift $ LB.writeFile ("data/" <> name <> "/" <> show relationId <> ".json") (encode value)

viaMemory :: ViaMemory
viaMemory = ViaMemory M.empty

viaDisk :: String -> ViaDisk
viaDisk namespace = ViaDisk namespace
