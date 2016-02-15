{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Environment
import Game (defaultGame, Game, gameHandleEvent)
import Linear (V4(V4))
import Renderer (renderGame)
import SDL
import qualified SDL.Image as Img

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
    destroyTexture tileset
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
