module Main where

{-
import Kawaii

main :: IO ()
main = runApp $ App
    { appTitle  = "Lonesome Space"
    , appMode   = Fullscreen
    , appScenes = [menu]
    }

menu :: Scene
menu = Scene
    { sceneUpdate = const (return Success)
    , sceneRender = return ()
    }
-}

import Control.Concurrent
import Kawaii.Core
-- import Kawaii.Engine -- different engines like SDL or other
import Relational

main :: IO ()
main = do
    putStrLn "Before game"
    runGame undefined
    putStrLn "After game"