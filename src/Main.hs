{-# LANGUAGE OverloadedStrings #-}

import SDL
import qualified SDL.Image as Img
import Control.Monad
import Linear (V4(V4))
import Control.Monad.Reader
import Control.Monad.State

import Game

main :: IO ()
main = do
    -- Initialize SDL an SDL_image
    initializeAll
    Img.initialize [Img.InitPNG]

    -- Fullscreen window with the default renderer
    window <- createWindow "LoneSome Space" defaultWindow { windowMode = FullscreenDesktop }
    renderer <- createRenderer window (-1) defaultRenderer
    
    -- Loading texture
    tileset <- Img.loadTexture renderer "assets/tileset.png"
    
    -- Some options for convenience
    disableScreenSaver
    rendererDrawColor renderer $= V4 0 0 0 0 -- black

    -- Prepare game
    clear renderer
    present renderer
    showWindow window

    -- Main loop
    runReaderT (mainLoop defaultGame) $ MkEnvironment
        { envWindow   = window
        , envRenderer = renderer
        , envTileset  = tileset
        }
    
    -- Cleanup
    destroyRenderer renderer
    destroyWindow window
    Img.quit
    quit

mainLoop :: Game -> Environment IO ()
mainLoop game = do
    -- Wait for any event
    event <- waitEvent
    
    -- Handle game event
    let (halt, newGame) = runState (gameHandleEvent event) game
    
    -- Render the game
    renderGame newGame

    -- Do it again
    unless halt $ mainLoop newGame
