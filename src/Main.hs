{-# LANGUAGE OverloadedStrings #-}

import SDL (($=))
import qualified SDL.Image as IM
import SDL.Video
import SDL.Event
import Control.Monad
import Linear.V4 (V4(V4))

main :: IO ()
main = do
    -- Initialize SDL_image
    IM.initialize [IM.InitPNG]

    -- Fullscreen window with the default renderer
    window <- createWindow "LoneSome Space" defaultWindow { windowMode = FullscreenDesktop }
    renderer <- createRenderer window (-1) defaultRenderer
    
    -- Loading texture
    _ <- IM.loadTexture renderer "assets/tileset.png"
    
    -- Some options for convenience
    disableScreenSaver
    rendererDrawColor renderer $= V4 0 0 0 0 -- black
    clear renderer
    present renderer
    showWindow window

    -- Main loop
    mainLoop window renderer
    
    -- Cleanup
    destroyRenderer renderer
    destroyWindow window
    IM.quit

mainLoop :: Window -> Renderer -> IO ()
mainLoop window renderer = do
    event <- waitEvent
    
    let quit = eventPayload event == QuitEvent

    clear renderer
    present renderer

    -- putStrLn "Event!"

    unless quit $ mainLoop window renderer
