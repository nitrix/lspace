module Main where

import Engine
import Control.Concurrent
import Control.Monad.IO.Class

main :: IO ()
main = withEngine $ do
    window <- engineCreateWindow "Lonesome Space" WindowFullscreen
    liftIO $ putStrLn "Hello World!"
    liftIO $ threadDelay 10000000
