{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Engine (engineHandleEvent, enginePokeIO, engineInit)
import Renderer (renderGame)
import SDL
import qualified SDL.Image as Img
import qualified SDL.TTF as Ttf
import System.Remote.Monitoring
import Types.Environment
import Types.Game (Game, defaultGame)

import Linear (V2(..), V4(..))
import Foreign.C.Types (CInt(..))

main :: IO ()
main = do
    initializeAll
    window <- createWindow "LoneSome Space" defaultWindow { windowMode = FullscreenDesktop }
    renderer <- createRenderer window (-1) defaultRenderer
    testTexture <- createTexture renderer RGBA8888 TextureAccessTarget (V2 1920 1080)

    rendererRenderTarget renderer $= Just testTexture
    rendererDrawColor renderer $= V4 255 0 0 0
    clear renderer
    rendererRenderTarget renderer $= Nothing
    rendererDrawColor renderer $= V4 0 0 0 0
    
    let loop = do
        events <- (:) <$> waitEvent <*> pollEvents
        clear renderer
        shouldHalts <- forM events $ \event -> do
            case eventPayload event of
                MouseMotionEvent d -> do
                    let pos = (fromIntegral <$> mouseMotionEventPos d)
                    copyEx renderer testTexture Nothing (Just $ Rectangle pos (V2 (CInt 1920) (CInt 1080))) 0 Nothing (V2 False False)
                    return False 
                QuitEvent -> return True
                _ -> return False
        present renderer
        unless (or shouldHalts) loop

    loop    
    quit

main' :: IO ()
main' = runInBoundThread $ Ttf.withInit $ do -- ^ TODO: GHC bug #11682 the bound thread is for ekg on ghci
    -- Initialize SDL an SDL_image
    initializeAll
    Img.initialize [Img.InitPNG]
    ekg <- forkServer "localhost" 8080

    -- Fullscreen window with the default renderer
    window <- createWindow "LoneSome Space" defaultWindow { windowMode = FullscreenDesktop }
    renderer <- createRenderer window (-1) defaultRenderer

    -- Loading texture
    tileset <- Img.loadTexture renderer "assets/tileset.png"

    -- Loading fonts
    font <- Ttf.openFont "assets/terminus.ttf" 16

    -- Some options for convenience
    disableScreenSaver

    -- Prepare all the things
    clear renderer
    present renderer
    showWindow window

    -- Main loop
    runReaderT (engineInit defaultGame >>= mainLoop) $ MkEnvironment
        { envWindow   = window
        , envRenderer = renderer
        , envTileset  = tileset
        , envFont     = font
        , envTileSize = 32
        }
    
    -- Cleanup
    killThread $ serverThreadId ekg
    Ttf.closeFont font
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
    -- (shouldHalts, newGame) <- runState (traverse engineHandleEvent events) =<< liftIO engineIO game
    (shouldHalts, newGame) <- runState (traverse engineHandleEvent events) <$> enginePokeIO game

    -- Then render the new game state
    renderGame newGame

    -- Continue doing it over and over again
    unless (or shouldHalts) (mainLoop newGame)
