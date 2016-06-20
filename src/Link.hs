module Link where

import Data.Aeson
import Data.IORef
import qualified Data.ByteString.Lazy as LB
import System.Directory
import System.Mem.Weak

import Types.Link

readLink :: FromJSON a => Link a -> IO (Maybe a)
readLink (LinkRef i r) = do
    xRef <- deRefWeak r
    case xRef of
        Nothing -> return Nothing
        Just x  -> readIORef x >>= return . Just
readLink (LinkId i) = do
    ok <- doesFileExist filepath
    if ok
    then do
        json <- LB.readFile filepath
        return $ decode json 
    else do
        print $ "Link #" ++ show i ++ " not found"
        return Nothing
    where
        filepath = "data/links/" ++ show i ++ ".json"
