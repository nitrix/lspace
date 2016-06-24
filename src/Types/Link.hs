module Types.Link where

import Data.Aeson as J
import Data.IORef
import System.Mem.Weak

data Link a = LinkId Int | LinkRef Int (Weak (IORef a))

instance FromJSON (Link a) where
    parseJSON (J.Number n) = do
        return $ LinkId $ truncate n
    parseJSON _ = error "Unable to parse Link json"
