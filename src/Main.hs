{-# LANGUAGE OverloadedStrings #-}

import SDL (($=))
import qualified SDL.Image as IM
import SDL.Video
import SDL.Event
import Control.Monad
import Linear (V2(V2), V4(V4))
import Linear.Affine (Point(P))
import Control.Monad.State
import Control.Lens

import Game

main :: IO ()
main = do
    -- Initialize SDL_image
    IM.initialize [IM.InitPNG]

    -- Fullscreen window with the default renderer
    window <- createWindow "LoneSome Space" defaultWindow { windowMode = FullscreenDesktop }
    renderer <- createRenderer window (-1) defaultRenderer
    
    -- Loading texture
    texture <- IM.loadTexture renderer "assets/tileset.png"
    
    -- Some options for convenience
    disableScreenSaver
    rendererDrawColor renderer $= V4 0 0 0 0 -- black
    clear renderer
    present renderer
    showWindow window

    -- Main loop
    mainLoop window renderer texture defaultGameState
    
    -- Cleanup
    destroyRenderer renderer
    destroyWindow window
    IM.quit

mainLoop :: Window -> Renderer -> Texture -> GameState -> IO ()
mainLoop window renderer texture gameState = do
    event <- waitEvent
    
    -- Handle events
    let (quit, newGameState) = runState (gameHandleEvent event) gameState
    
    -- Debugging
    putStrLn $ "Counter: " ++ show (newGameState ^. counter)
    
    -- Render camera
    clear renderer
    let src = Rectangle (P $ V2 0 0) (V2 32 32)
    let dst = Rectangle (P $ V2 0 0) (V2 32 32)
    copyEx renderer texture (Just src) (Just dst) 0 Nothing (V2 False False)
    present renderer

    -- Do it again
    unless quit $ mainLoop window renderer texture newGameState
