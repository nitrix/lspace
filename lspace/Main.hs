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

import Kawaii

main :: IO ()
main = runApp $ App
    { appTitle      = "Lonesome Space"
    , appMode       = Windowed 1024 768
    , appPathAssets = "assets"
    , appUis        = [Ui updateMainMenu renderMainMenu]
    }

updateMainMenu :: Event -> Game Result
updateMainMenu (EventKeyPressed ScancodeEscape _) = return Terminate
updateMainMenu _ = return Skip

renderMainMenu :: Renderer ()
renderMainMenu = return ()

updateGame :: Event -> Game Result
updateGame _ = return Skip

renderGame :: Renderer ()
renderGame = return ()

{-
import Relational
import Debug.Trace

main :: IO ()
main = runRelational (viaDisk "test") $ do
    foo <- createRelation ([1,2,3] :: [Int])
    updateRelation foo [69,42,18]
    updateRelation foo [5,5,5]
    result <- readRelation foo
    traceShow result $ return ()
-}