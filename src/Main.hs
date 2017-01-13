module Main where

import Engine

main :: IO ()
main = runApp $ App
    { appTitle  = "Lonesome Space"
    , appMode   = Windowed 800 600
    , appScenes = [menu]
    }

menu :: Scene
menu = Scene
    { sceneUpdate = const (return Success)
    , sceneRender = return ()
    }
