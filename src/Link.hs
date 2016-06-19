module Link where

import Data.IORef
import System.Directory
import System.Mem.Weak

import Types.Link

readLink :: Link a -> IO (Maybe a)
readLink (LinkRef i r) = do
    xRef <- deRefWeak r
    case xRef of
        Nothing -> return Nothing
        Just x  -> readIORef x >>= return . Just
readLink (LinkId i) = do
    ok <- doesFileExist filepath
    if ok
    then do
        json <- readFile filepath
        return Nothing -- TODO
    else do
        print $ "Link #" ++ show i ++ " not found"
        return Nothing
    where
        filepath = "data/links/" ++ show i ++ ".json"
