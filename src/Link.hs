module Link where

import qualified Data.Aeson as J
import Data.IORef
import qualified Data.ByteString.Lazy as LB
import System.Directory
import System.Mem.Weak

import Types.Link

readLink :: J.FromJSON a => Link a -> IO (Maybe a)
readLink (LinkRef i r) = do
    xRef <- deRefWeak r
    case xRef of
        Nothing -> readLink (LinkId i)
        Just x  -> readIORef x >>= return . Just
readLink (LinkId i) = do
    ok <- doesFileExist filepath
    if ok
    then do
        json <- LB.readFile filepath
        return $ J.decode json 
    else do
        print $ "Link #" ++ show i ++ " not found"
        return Nothing
    where
        filepath = "data/links/" ++ show i ++ ".json"
