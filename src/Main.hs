{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Environment
import Game (defaultGame, Game, gameHandleEvent)
import Linear (V4(V4))
import Renderer (renderGame)
import SDL
import qualified SDL.Image as Img
import System.Remote.Monitoring

main :: IO ()
main = runInBoundThread $ do -- ^ TODO: GHC bug #11682
    -- Initialize SDL an SDL_image
    initializeAll
    Img.initialize [Img.InitPNG]
    ekg <- forkServer "localhost" 8080

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

    runReaderT (mainLoop defaultGame) $ MkEnvironment
        { envWindow   = window
        , envRenderer = renderer
        , envTileset  = tileset
        }

    -- Cleanup
    killThread $ serverThreadId ekg
    destroyTexture tileset
    destroyRenderer renderer
    destroyWindow window
    Img.quit
    quit

mainLoop :: Game -> EnvironmentT IO ()
mainLoop game = do
    -- Waiting for events; those can only be called in the same thread that set up the video mode
    events <- (:) <$> waitEvent <*> pollEvents

    -- As an optimisation, prevent chocking by processing all the queued up events at once
    -- Previously was:
    --     let (halt, newGame) = runState (gameHandleEvent event) game
    let (shouldHalts, newGame) = runState (traverse gameHandleEvent events) game

    -- Then render the new game state
    renderGame newGame

    -- Continue doing it over and over again
    unless (or shouldHalts) (mainLoop newGame)
