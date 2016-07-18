module Types.Link where

import Data.Aeson as J
import Data.IORef
import System.Mem.Weak
import System.IO.Unsafe

data Link a = MkLink (IORef (Int, Maybe (Weak (IORef a))))

instance FromJSON (Link a) where
    parseJSON (J.Number n) = do
        return $ MkLink $ unsafePerformIO $ newIORef $ (truncate n, Nothing)
    parseJSON _ = error "Unable to parse Link json"

instance ToJSON (Link a) where
    toJSON (MkLink lnk) = Number $ fromIntegral $ fst $ unsafePerformIO $ readIORef lnk
