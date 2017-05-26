module Main where

import Kawaii

main :: IO ()
main = runApp $ defaultApp
    { appTitle     = "Lonesome Space"
    , appMode      = Windowed 1024 768
    , appStages    = [Stage updateGame renderGame]
    , appGrabInput = True
    }

updateMainMenu :: Event -> Updating ()
updateMainMenu _ = return Skip

renderMainMenu :: Rendering ()
renderMainMenu = return ()

updateGame :: Event -> Updating ()
updateGame (EventKeyPressed ScancodeEscape _) = return Terminate
updateGame (EventKeyPressed ScancodeSpace _) = return Success
updateGame _ = return Skip

renderGame :: Rendering ()
renderGame = drawTile "tileset" 1 2 0 0 0 0 -- Say hi little astronaut!

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