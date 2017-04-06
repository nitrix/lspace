module Main where

import Debug.Trace

{-
import Kawaii

main :: IO ()
main = runApp $ App
    { appTitle  = "Lonesome Space"
    , appMode   = Windowed 800 600 -- Fullscreen
    , appScenes = [menu]
    }

menu :: Scene
menu = Scene
    { sceneUpdate = const (return Success)
    , sceneRender = return ()
    }

menu = Scene menuUpdate menuRender

menuUpdate :: Event -> Engine Result
menuUpdate event = do
    case event of
        EventKeyPressed key -> if key == KeycodeEscape then return Terminate else return Success
        _                   -> return Skip

menuRender :: Engine ()
menuRender = return ()
-}

-- import Kawaii
import Relational

{-
main :: IO ()
main = runApp $ App
    { appTitle = "Lonesome Space"
    , appMode  = Windowed 1024 768
    , appUis   = [uiMainMenu]
    }

uiMainMenu :: Ui
uiMainMenu = const $ return Success 
-}

main :: IO ()
main = runRelational (viaDisk "test") $ do
    foo <- createRelation ([1,2,3] :: [Int])
    updateRelation foo [69,42,18]
    result <- readRelation foo
    traceShow result $ return ()