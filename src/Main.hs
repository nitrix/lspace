{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import SDL
import qualified SDL.Image as Img
import qualified SDL.TTF as Ttf

import Engine      (engineHandleEvent, engineInit)
import Renderer    (renderGame)
import Cache       (defaultCache)
import Environment (Environment(..), EnvironmentT)
import Game        (GameState, runGame)
import Link        (initContext, saveContext, readLink, writeLink, defaultLink)

main :: IO ()
main = runInBoundThread $ Ttf.withInit $ do -- ^ TODO: GHC bug #11682 the bound thread is for ekg on ghci
    -- Initialize SDL an SDL_image
    initializeAll
    Img.initialize [Img.InitPNG]

    -- Fullscreen window with the default renderer, tileset and fonts
    window   <- createWindow "LoneSome Space" defaultWindow { windowMode = FullscreenDesktop }
    renderer <- createRenderer window (-1) defaultRenderer
    tileset  <- Img.loadTexture renderer "assets/tileset.png"
    font     <- Ttf.openFont "assets/terminus.ttf" 16

    -- Some options for convenience
    disableScreenSaver
    cursorVisible $= False

    -- Prepare all the things
    clear renderer
    present renderer
    showWindow window

    -- Create cache and relational context
    cacheRef <- newIORef defaultCache
    context  <- initContext (Just 1000) "data/demo/"
    
    -- Load game
    game <- fromJust <$> readLink context defaultLink

    -- Main loop
    runReaderT (engineInit game >>= mainLoop) $ MkEnvironment
        { envCacheRef = cacheRef
        , envContext  = context
        , envFont     = font
        , envRenderer = renderer
        , envTileset  = tileset
        , envTileSize = 32
        , envWindow   = window
        }
        
    -- Cleanup cache
    writeIORef cacheRef defaultCache
    saveContext context

    -- Cleanup
    Ttf.closeFont font
    destroyTexture tileset
    destroyRenderer renderer
    destroyWindow window
    Img.quit
    quit

-- Main loop
mainLoop :: GameState -> EnvironmentT IO ()
mainLoop game = do
    env <- ask -- TODO: this is ugly

    -- Waiting for events; those can only be called in the same thread that set up the video mode
    events <- (:) <$> waitEvent <*> pollEvents

    -- As an optimisation, prevent chocking by processing all the queued up events at once
    (shouldHalts, newGame) <- lift $ runGame env game $ traverse engineHandleEvent events

    -- Then render the new game state
    renderGame newGame

    -- Continue doing it over and over again
    if (or $ catMaybes $ sequence shouldHalts)
    then mainLoop newGame
    else gameWriteLink defaultLink newGame