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
import Control.Monad.State
import Debug.Trace

data LonesomeSpace = LonesomeSpace
    { test :: Int
    } deriving Show

main :: IO ()
main = runApp $ App
    { appTitle       = "Lonesome Space"
    , appMode        = Windowed 1024 768
    , appPathAssets  = "assets"
    , appUis         = [Ui updateMainMenu renderMainMenu]
    , appCustomState = LonesomeSpace 42
    }

updateMainMenu :: Event -> Updater LonesomeSpace Result
updateMainMenu (EventKeyPressed ScancodeEscape _) = return Terminate
updateMainMenu (EventKeyPressed ScancodeSpace _) = do
    cs <- get
    trace (show cs) $ return Success
updateMainMenu _ = return Skip

renderMainMenu :: Renderer LonesomeSpace ()
renderMainMenu = do
    return $ ()

updateGame :: Event -> Updater LonesomeSpace Result
updateGame _ = return Skip

renderGame :: Renderer LonesomeSpace ()
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