module Main where

import Engine
-- import Control.Concurrent
-- import Control.Monad.IO.Class

data GameState = GameState Int

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
