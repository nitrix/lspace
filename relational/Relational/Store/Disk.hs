{-# LANGUAGE TemplateHaskell #-}

module Relational.Store.Disk where

import Control.Monad
import Control.Monad.State (lift)
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Label (mkLabel)
import Data.Label.Monadic (gets, modify, puts)
import Data.Monoid
import System.Directory

import Relational.Store

data ViaDisk = ViaDisk { _diskNamespace :: String, _diskIncrement :: Integer }
mkLabel ''ViaDisk

instance Store ViaDisk where
    storeInit = do
        name <- gets diskNamespace
        exists <- lift $ doesFileExist $ "data/" <> name <> "/increment.json"
        lift $ print exists
        increment <- case exists of
            False -> return 0
            True  -> do
                content <- lift $ LB.readFile ("data/" <> name <> "/increment.json")
                case decode content of
                    Just i  -> return i
                    Nothing -> error "The increment on disk is a malformed JSON"
        lift $ print increment
        puts diskIncrement increment
    
    storeQuit = do
        name <- gets diskNamespace
        increment <- gets diskIncrement
        lift $ createDirectoryIfMissing True ("data/" <> name)
        lift $ LB.writeFile ("data/" <> name <> "/increment.json") (encode increment)

    storeCreate value = do
        relationId <- gets diskIncrement
        lift $ print relationId
        modify diskIncrement (+1)
        storeUpdate relationId value
        return relationId

    storeRead relationId = do
        name <- gets diskNamespace
        result <- lift $ LB.readFile ("data/" <> name <> "/" <> show relationId <> ".json")
        case decode result of
            Just r  -> return r
            Nothing -> error "The relation requested is a malformed JSON"

    storeUpdate relationId value = do
        name <- gets diskNamespace
        lift $ createDirectoryIfMissing True ("data/" <> name)
        lift $ LB.writeFile ("data/" <> name <> "/" <> show relationId <> ".json") (encode value)

    storeDelete relationId = do
        name <- gets diskNamespace
        exists <- lift $ doesFileExist $ "data/" <> name <> "/" <> show relationId <> ".json"
        when exists $ lift $ removeFile $ "data/" <> name <> "/" <> show relationId <> ".json"

    storeCacheable = const True

viaDisk :: String -> ViaDisk
viaDisk namespace = ViaDisk namespace 0