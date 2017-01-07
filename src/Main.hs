module Main where

import Engine
-- import Control.Concurrent
-- import Control.Monad.IO.Class

main :: IO ()
main = runApp $ App
    { appTitle  = "Lonesome Space"
    , appMode   = Windowed 800 600
    , appScenes = [menu]
    , appUpdate = undefined
    , appRender = undefined
    }

menu :: Scene s
menu = Scene
    { sceneUpdate = undefined
    , sceneRender = undefined
    }
