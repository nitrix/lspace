module Kawaii.Core
    ( App(..)
    , Mode(..)
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

data Direction = North | South | East | West deriving (Eq, Show)

data Mode = Fullscreen | Windowed Int Int
data App c = App
    { appTitle       :: String
    , appMode        :: Mode
    , appStages      :: [Stage c]
    -- , appPathData   :: FilePath
    , appPathAssets  :: FilePath
    , appCustomState :: c
    }

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
                           Windowed w h -> Sdl.defaultWindow { Sdl.windowInitialSize = Sdl.V2 (fromIntegral w) (fromIntegral h) }

    -- SDL window and renderer
    window   <- Sdl.createWindow (T.pack $ appTitle app) windowConfig
    renderer <- Sdl.createRenderer window (-1) Sdl.defaultRenderer

    -- Few SDL settings; this might be a little too arbitrary
    Sdl.disableScreenSaver
    Sdl.cursorVisible Sdl.$= False
    Sdl.showWindow window
    -- Mix.setChannels 8

    -- Load assets
    assets <- loadAssets renderer (appPathAssets app)

    -- Thread communication
    eventChan    <- newChan      -- Event channel (SDL events to be processed by the logic thread)
    gameStateSV  <- newEmptySV   -- Scene state sampling variable (contains a snapshot of the current scene state to render)
    logicEndMVar <- newEmptyMVar -- Logic thread end signal
    
    -- Threads
    -- TIDO: This block is very `eww`
    let stages = S.fromList (appStages app)
    let gameState = defaultGameState
    let customState = appCustomState app
    logicThreadId   <- forkOS (logicThread eventChan gameState customState gameStateSV logicEndMVar stages)
    renderThreadId  <- forkOS (renderThread gameStateSV renderer assets)
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

    -- Cleanup assets
    mixerHalt
    unloadAssets assets

    -- Cleanup
    Mix.closeAudio
    Ttf.quit
    Sdl.quit

logicThread :: Chan Sdl.EventPayload -> GameState -> c -> MSampleVar GameState -> MVar () -> S.Seq (Stage c) -> IO ()
logicThread eventChan gameState customState gameStateSV logicEndMVar stages = do
    sdlEvent <- liftIO (readChan eventChan)
    if (sdlEvent == Sdl.QuitEvent)
    then do
        -- TODO: Save game state
        liftIO $ putMVar logicEndMVar ()
    else do
        let event = convertSdlEvent sdlEvent
        -- let game = uiHandleEvent customState uis event
        -- ((newCustomState, newUis), newGameState) <- runStateT (unwrapGame game) gameState
        -- let (newCustomState, newStages, newGameState) = (customState, stages, gameState) -- TODO: temporary
        ((newStages, newCustomState), newGameState) <- runStateT (unwrapGame $ stageBubbleEvent event customState stages) gameState

        writeSV gameStateSV newGameState
        logicThread eventChan newGameState newCustomState gameStateSV logicEndMVar newStages

renderThread :: MSampleVar GameState -> Sdl.Renderer -> Assets -> IO ()
renderThread sceneStateSV renderer _assets = forever $ do
    -- Wait for the game state to change
    _gameState <- readSV sceneStateSV

    -- Clear the screen
    Sdl.rendererDrawColor renderer Sdl.$= Sdl.V4 0 0 0 255
    Sdl.clear renderer
    -- Present the result
    Sdl.present renderer

    -- Render that game state
    -- renderGame renderer assets gameState

networkThread :: IO ()
networkThread = forever $ do
    threadDelay 1000000