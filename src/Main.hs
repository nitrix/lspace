module Main where

import Engine

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
