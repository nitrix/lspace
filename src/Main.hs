{-# LANGUAGE OverloadedStrings #-}

import SDL
import qualified SDL.Image as IM
import Control.Monad
import Linear (V2(V2), V4(V4))
import Linear.Affine (Point(P))
import Control.Monad.State
import Control.Lens

import Game
import Camera
import Coordinate

main :: IO ()
main = do
    -- Initialize SDL an SDL_image
    initializeAll
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
    SDL.quit

mainLoop :: Window -> Renderer -> Texture -> GameState -> IO ()
mainLoop window renderer texture gameState = do
    event <- waitEvent
    
    -- Handle events
    let (halt, newGameState) = runState (gameHandleEvent event) gameState
    
    -- Debugging
    putStrLn $ "Counter: " ++ show (newGameState ^. counter)
    
    -- Render camera
    clear renderer
    let x = fromIntegral $ newGameState ^. camera . cameraCoordinate . coordinateX
    let y = fromIntegral $ newGameState ^. camera . cameraCoordinate . coordinateY
    let src = Rectangle (P $ V2 0 0) (V2 32 32)
    let dst = Rectangle (P $ V2 (x*32) (y*32)) (V2 32 32)
    copyEx renderer texture (Just src) (Just dst) 0 Nothing (V2 False False)
    present renderer

    -- Do it again
    unless halt $ mainLoop window renderer texture newGameState
