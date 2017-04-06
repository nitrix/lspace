module Relational.Store.Disk where

import Control.Monad.State (lift)
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Label.Monadic (gets)
import Data.Monoid
import System.Directory

import Relational.Store
import Relational.Types

instance Store ViaDisk where
    storeRead relationId = do
        name   <- gets diskNamespace
        result <- lift $ LB.readFile ("data/" <> name <> "/" <> show relationId <> ".json")
        -- TODO: catch not found IO exception
        case decode result of
            Just r  -> return r
            Nothing -> error "The relation requested is a malformed JSON"
    storeUpdate relationId value = do
        name <- gets diskNamespace
        lift $ createDirectoryIfMissing True ("data/" <> name)
        lift $ LB.writeFile ("data/" <> name <> "/" <> show relationId <> ".json") (encode value)
    storeCacheable = const True

viaDisk :: String -> ViaDisk
viaDisk namespace = ViaDisk namespace