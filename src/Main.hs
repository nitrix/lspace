{-# LANGUAGE OverloadedStrings #-}

import SDL (($=))
import SDL.Video
import SDL.Video.Renderer
import SDL.Event
import Control.Monad
import Linear.V4 (V4(V4))
import Data.Word

main = do
    -- x <- getDisplays
    -- print x    
    -- idealWindowSize <- displayModeSize . head . displayModes . head <$> getDisplays TODO: doesnt work without initialized?
    window <- createWindow "LoneSome Space" defaultWindow { windowMode = FullscreenDesktop }
    renderer <- createRenderer window (-1) defaultRenderer
    
    showWindow window
    disableScreenSaver

    rendererDrawColor renderer $= V4 0 0 0 0 -- black
    mainLoop renderer

    destroyRenderer renderer
    destroyWindow window

mainLoop renderer = do
    clear renderer
    present renderer

    event <- waitEvent
    
    let quit = eventPayload event == QuitEvent

    -- putStrLn "Event!"
    
    unless quit $ mainLoop renderer
