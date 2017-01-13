module Main where

import Engine

main :: IO ()
main = runApp $ App
    { appTitle  = "Lonesome Space"
    , appMode   = Windowed 800 600
    , appScenes = [menu]
    , appUpdate = undefined
    , appRender = undefined
    }

menu :: Scene
menu = Scene
    { sceneUpdate = undefined
    , sceneRender = undefined
    }
