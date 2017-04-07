module Main where

{-
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

{-
import Kawaii

main :: IO ()
main = runApp $ App
    { appTitle = "Lonesome Space"
    , appMode  = Windowed 1024 768
    , appUis   = [uiMainMenu, uiGame]
    }

uiMainMenu :: Ui
uiMainMenu = const $ return Skip

uiGame :: Ui
uiGame = const $ return Success
-}

import Relational
import Debug.Trace

main :: IO ()
main = runRelational (viaDisk "test") $ do
    foo <- createRelation ([1,2,3] :: [Int])
    updateRelation foo [69,42,18]
    updateRelation foo [5,5,5]
    result <- readRelation foo
    traceShow result $ return ()