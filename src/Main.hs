{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import SDL
import qualified SDL.Image as Img
import qualified SDL.TTF as Ttf

import Engine      (engineHandleEvent, engineInit, engineLoadGame)
import Renderer    (renderGame)
import Cache       (defaultCache)
import Environment (Environment(..), EnvironmentT)
import Game        (GameState, runGame)
import Link        (saveAllLinks)

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

    -- Create cache
    cacheRef <- newIORef defaultCache
    
    -- Load game
    game <- engineLoadGame "demo"

    -- Main loop
    runReaderT (engineInit game >>= mainLoop) $ MkEnvironment
        { envCacheRef = cacheRef
        , envFont     = font
        , envRenderer = renderer
        , envTileset  = tileset
        , envTileSize = 32
        , envWindow   = window
        }
    
    -- Cleanup cache
    writeIORef cacheRef defaultCache
    saveAllLinks

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
    -- (shouldHalts, newGame) <- lift $ runStateT (traverse (engineHandleEvent env) events) game
    (shouldHalts, newGame) <- lift $ runGame (traverse (engineHandleEvent env) events) game

    -- Then render the new game state
    renderGame newGame

    -- Continue doing it over and over again
    unless (or $ catMaybes $ sequence shouldHalts) (mainLoop newGame)
