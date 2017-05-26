module Kawaii.Core
    ( App(..)
    , Mode(..)
    , defaultApp
    , runApp
    ) where

import Control.Concurrent
import Control.Concurrent.MSampleVar
import Control.Monad.Loops
import Control.Monad.State

import qualified Data.Sequence as S
import qualified Data.Text     as T
import qualified SDL           as Sdl
import qualified SDL.Image     as Img
import qualified SDL.Mixer     as Mix
import qualified SDL.TTF       as Ttf

import Kawaii.Assets
import Kawaii.Event
import Kawaii.Game
import Kawaii.Mixer
import Kawaii.Stage

-- TODO: Maybe the Kawaii engine should handle the mouse events for the camera handling?

data Mode = Fullscreen | Windowed Int Int
data App c = App
    { appTitle       :: String
    , appMode        :: Mode
    , appStages      :: [Stage c]
    , appGrabInput   :: Bool
    -- , appPathData   :: FilePath -- TODO: That's for when we have Relational
    , appPathAssets  :: FilePath
    , appCustomState :: c
    }

defaultApp :: App ()
defaultApp = App
    { appTitle       = "Kawaii application"
    , appMode        = Fullscreen
    , appStages      = [Stage defaultUpdateStage defaultRenderStage]
    , appGrabInput   = False
    , appPathAssets  = "assets"
    , appCustomState = ()
    }
    where
        defaultUpdateStage :: Event -> Updating ()
        defaultUpdateStage (EventKeyPressed Sdl.ScancodeEscape _) = return Terminate
        defaultUpdateStage _ = return Success

        defaultRenderStage :: Rendering ()
        defaultRenderStage = return ()

runApp :: App c -> IO ()
runApp app = runInBoundThread $ do -- Fixes a GHCi bug where the main thread isn't bound
    -- We're going to need SDL
    Sdl.initializeAll
    Img.initialize [Img.InitPNG]
    void Ttf.init
    Mix.openAudio (Mix.Audio 48000 Mix.FormatS16_LSB Mix.Mono) 1024 -- 44100 Hz

    -- Creating the application window
    desktopSize <- Sdl.displayBoundsSize . head <$> Sdl.getDisplays
    let windowConfig = case appMode app of
                        -- We workaround that neither the `fake` or `real` fullscreen modes play nice with Alt-tab
                        -- Fullscreen   -> Sdl.defaultWindow { Sdl.windowMode = Sdl.FullscreenDesktop }
                           Fullscreen   -> Sdl.defaultWindow { Sdl.windowMode = Sdl.Windowed, Sdl.windowInitialSize = desktopSize, Sdl.windowBorder = False }
                           Windowed w h -> Sdl.defaultWindow { Sdl.windowInitialSize = Sdl.V2 (fromIntegral w) (fromIntegral h), Sdl.windowInputGrabbed = appGrabInput app }

    -- SDL window and renderer
    window   <- Sdl.createWindow (T.pack $ appTitle app) windowConfig
    renderer <- Sdl.createRenderer window (-1) Sdl.defaultRenderer

    -- Few SDL settings; this might be a little too arbitrary
    Sdl.disableScreenSaver
    -- Sdl.cursorVisible Sdl.$= False
    Sdl.showWindow window
    -- Mix.setChannels 8

    -- Load assets
    assets <- loadAssets renderer (appPathAssets app)

    -- Thread communication
    eventChan    <- newChan      -- Event channel (SDL events to be processed by the logic thread)
    renderSV     <- newEmptySV   -- GameState and stages sampling variable (contains a snapshot of the current game state and stages to render)
    logicEndMVar <- newEmptyMVar -- Logic thread end signal
    
    -- Threads
    -- TODO: This block is very `eww`
    let stages = S.fromList (appStages app)
    let gameState = defaultGameState
    let customState = appCustomState app
    logicThreadId   <- forkOS (logicThread eventChan gameState customState renderSV logicEndMVar stages)
    renderThreadId  <- forkOS (renderThread renderSV renderer assets)
    networkThreadId <- forkOS (networkThread)

    -- Main thread with event collecting
    void $ iterateWhile (/= Sdl.QuitEvent) $ do
        event <- Sdl.eventPayload <$> Sdl.waitEvent
        writeChan eventChan event
        return event
    
    -- Waiting for some important threads to finish
    readMVar logicEndMVar
    
    -- Killing all the threads
    killThread logicThreadId
    killThread renderThreadId
    killThread networkThreadId

    -- Cleanup assets.
    -- We must be careful to stop anything that might be playing before unloading them
    mixerHalt
    unloadAssets assets

    -- Cleanup
    Mix.closeAudio
    Ttf.quit
    Sdl.quit

logicThread :: Chan Sdl.EventPayload -> GameState -> c -> MSampleVar (GameState, S.Seq (Stage c), c) -> MVar () -> S.Seq (Stage c) -> IO () -- TODO: Maybe it's time to use RecordWildcards
logicThread eventChan gameState customState renderSV logicEndMVar stages = do
    event <- convertSdlEvent <$> liftIO (readChan eventChan)
    if (event == EventQuit)
    then do
        -- TODO: Save game state
        liftIO $ putMVar logicEndMVar ()
    else do
        ((newStages, newCustomState), newGameState) <- runGame (stageBubbleEvent event customState stages) gameState

        writeSV renderSV (newGameState, stages, newCustomState)
        logicThread eventChan newGameState newCustomState renderSV logicEndMVar newStages

renderThread :: MSampleVar (GameState, S.Seq (Stage c), c) -> Sdl.Renderer -> Assets -> IO ()
renderThread renderSV renderer assets = forever $ do
    -- Wait for our sample to render
    (gameState, stages, customState) <- readSV renderSV

    -- Clear the screen
    Sdl.rendererDrawColor renderer Sdl.$= Sdl.V4 0 0 0 255
    Sdl.clear renderer

    -- Render the different stages
    stageRender stages renderer assets gameState customState

    -- Present the result
    Sdl.present renderer

networkThread :: IO ()
networkThread = forever $ threadDelay 1000000