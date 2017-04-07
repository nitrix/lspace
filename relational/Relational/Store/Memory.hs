{-# LANGUAGE TemplateHaskell #-}

module Relational.Store.Memory where

import Data.Dynamic
import Data.Label (mkLabel)
import Data.Label.Monadic (gets, modify)
import qualified Data.Map as M

import Relational.Store

data ViaMemory = ViaMemory { _memoryMapping :: M.Map Integer Dynamic
                           , _memoryIncrement :: Integer
                           }

mkLabel ''ViaMemory

instance Store ViaMemory where
    storeInit = return ()
    storeQuit = return ()
    storeCreate value = do
        relationId <- gets memoryIncrement
        modify memoryIncrement (+1)
        storeUpdate relationId value
        return relationId
    storeRead relationId = do
        mm <- gets memoryMapping
        case M.lookup relationId mm of
            Just found -> do
                case fromDynamic found of
                    Just value -> return value
                    Nothing    -> error "The value in memory has the wrong representation for the relation requested"
            Nothing -> error "The relation requested was not found in memory"
    storeUpdate relationId value = modify memoryMapping (M.insert relationId (toDyn value))
    storeDelete relationId = modify memoryMapping (M.delete relationId)
    storeCacheable = const False

viaMemory :: ViaMemory
viaMemory = ViaMemory M.empty 0