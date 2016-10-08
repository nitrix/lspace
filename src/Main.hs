{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.IORef
import qualified SDL       as Sdl
import qualified SDL.Image as Img
import qualified SDL.TTF   as Ttf

import Cache
import Engine
import Environment
import Renderer

main :: IO ()
main = do
    -- Initialize SDL an SDL_image
    Sdl.initializeAll
    Img.initialize [Img.InitPNG]
    void Ttf.init

    -- Fullscreen window with the default renderer, tileset and fonts
    window   <- Sdl.createWindow "LoneSome Space" Sdl.defaultWindow { Sdl.windowMode = Sdl.FullscreenDesktop }
    renderer <- Sdl.createRenderer window (-1) Sdl.defaultRenderer
    tileset  <- Img.loadTexture renderer "assets/tileset.png"
    font     <- Ttf.openFont "assets/terminus.ttf" 16

    -- Some options for convenience
    Sdl.disableScreenSaver
    Sdl.cursorVisible Sdl.$= False

    -- Prepare all the things
    Sdl.clear renderer
    Sdl.present renderer
    Sdl.showWindow window
    
    -- Create cache
    cacheRef <- newIORef defaultCache

    -- Run the mainLoop engine for the game "demo" with the supplied environment
    withEngine mainLoop "demo" $ MkEnvironment
        { envFont     = font
        , envRenderer = renderer
        , envCacheRef = cacheRef
        , envTileset  = tileset
        , envTileSize = 32
        , envWindow   = window
        }
    
    -- Destroy cache
    writeIORef cacheRef defaultCache
    
    -- Cleanup
    Ttf.closeFont font
    Ttf.quit
    Img.quit
    Sdl.destroyTexture tileset
    Sdl.destroyRenderer renderer
    Sdl.destroyWindow window
    Sdl.quit

-- Main loop
mainLoop :: Engine ()
mainLoop = do
    -- Waiting for events; those can only be called in the same thread that set up the video mode
    events <- (:) <$> Sdl.waitEvent <*> Sdl.pollEvents

    -- As an optimisation, prevent chocking by processing all the queued up events at once
    shouldHalts <- embedGame $ traverse engineHandleEvent events
    
    -- Then render the game
    renderGame

    -- Continue doing it over and over again
    unless (or shouldHalts) mainLoop
