{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import SDL
import qualified SDL.Image as Img
import qualified SDL.TTF as Ttf

import Engine            (engineHandleEvent, engineInit)
import Renderer          (renderGame)
import Types.Cache       (defaultCache, cacheStars)
import Types.Environment (Environment(..), EnvironmentT)
import Types.Game        (Game)

main :: IO ()
main = runInBoundThread $ Ttf.withInit $ do -- ^ TODO: GHC bug #11682 the bound thread is for ekg on ghci
    -- Initialize SDL an SDL_image
    initializeAll
    Img.initialize [Img.InitPNG]

    -- Fullscreen window with the default renderer
    window <- createWindow "LoneSome Space" defaultWindow { windowMode = FullscreenDesktop }
    renderer <- createRenderer window (-1) defaultRenderer

    -- Loading texture
    tileset <- Img.loadTexture renderer "assets/tileset.png"

    -- Loading fonts
    font <- Ttf.openFont "assets/terminus.ttf" 16

    -- Some options for convenience
    disableScreenSaver
    cursorVisible $= False

    -- Prepare all the things
    clear renderer
    present renderer
    showWindow window

    -- Creating cache
    cacheRef <- newIORef defaultCache

    -- Load game
    let game = MkGame {
    }

    -- Main loop
    runReaderT (engineInit game >>= mainLoop) $ MkEnvironment
        { envCacheRef = cacheRef
        , envFont     = font
        , envRenderer = renderer
        , envTileset  = tileset
        , envTileSize = 32
        , envWindow   = window
        }
    
    -- Deleting cache
    stars <- view cacheStars <$> readIORef cacheRef
    mapM_ destroyTexture stars

    -- Cleanup
    Ttf.closeFont font
    destroyTexture tileset
    destroyRenderer renderer
    destroyWindow window
    Img.quit
    quit

-- Main loop
mainLoop :: Game -> EnvironmentT IO ()
mainLoop game = do
    env <- ask -- TODO: this is ugly

    -- Waiting for events; those can only be called in the same thread that set up the video mode
    events <- (:) <$> waitEvent <*> pollEvents

    -- As an optimisation, prevent chocking by processing all the queued up events at once
    (shouldHalts, newGame) <- lift $ runStateT (traverse (engineHandleEvent env) events) game

    -- Then render the new game state
    renderGame newGame

    -- Continue doing it over and over again
    unless (or shouldHalts) (mainLoop newGame)
