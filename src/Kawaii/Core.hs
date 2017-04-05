module Kawaii.Core
    ( App(..)
    , Mode(..)
    , Result(..)
    , runApp
    ) where

import Control.Concurrent
import Control.Concurrent.MSampleVar
import Control.Monad.Loops
import Control.Monad.State
import Data.IORef
import Foreign.Storable
import Foreign.Marshal.Alloc

import qualified Data.Text     as T
import qualified Data.Map      as M
import qualified SDL           as Sdl
import qualified SDL.Image     as Img
import qualified SDL.Mixer     as Mix
import qualified SDL.Raw.Event as Raw
import qualified SDL.Raw.Types as Raw
import qualified SDL.TTF       as Ttf

import Kawaii.Assets
import Kawaii.Game
import Kawaii.Mixer
import Kawaii.Renderer
import Kawaii.Ui

data Direction = North | South | East | West deriving (Eq, Show)

data Mode = Fullscreen | Windowed Int Int
data App = App
    { appTitle  :: String
    , appMode   :: Mode
    , appUis    :: [Ui]
    -- TODO , appAssets
    }

runApp :: App -> IO ()
runApp app = runInBoundThread $ do -- Fixes a GHCi bug where the main thread isn't bound
    -- We're going to need SDL
    Sdl.initializeAll
    Img.initialize [Img.InitPNG]
    void Ttf.init
    Mix.openAudio (Mix.Audio 48000 Mix.FormatS16_LSB Mix.Mono) 1024 -- 44100 Hz

    -- Workaround the fake and real fullscreen modes not playing nice with Alt-tab
    desktopSize <- Sdl.displayBoundsSize . head <$> Sdl.getDisplays
    let windowConfig = case appMode app of
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
    assets <- loadAssets renderer

    -- Thread communication
    eventChan    <- newChan      -- Event channel (SDL events to be processed by the logic thread)
    gameStateSV  <- newEmptySV   -- Game state sampling variable (contains a snapshot of the game state to render)
    logicEndMVar <- newEmptyMVar -- Logic thread end signal
    
    -- Threads
    logicThreadId   <- forkOS (logicThread eventChan defaultGameState gameStateSV logicEndMVar)
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

logicThread :: Chan Sdl.EventPayload -> GameState -> MSampleVar GameState -> MVar () -> IO ()
logicThread eventChan gameState gameStateSV logicEndMVar = do
    newGameState <- execStateT (unwrapGame $ gameLogic eventChan logicEndMVar) gameState
    writeSV gameStateSV newGameState
    logicThread eventChan gameState gameStateSV logicEndMVar

gameLogic :: Chan Sdl.EventPayload -> MVar () -> Game ()
gameLogic eventChan logicEndMVar = do
    event <- liftIO (readChan eventChan)
    case event of
        -- --------------------------- Testing movement --------------------------------
        {-
        Sdl.KeyboardEvent (Sdl.KeyboardEventData _ Sdl.Released False (Sdl.Keysym _ _ _)) -> stopPlayer
        Sdl.KeyboardEvent (Sdl.KeyboardEventData _ Sdl.Pressed False (Sdl.Keysym Sdl.ScancodeW _ _)) -> movePlayer exchange North
        Sdl.KeyboardEvent (Sdl.KeyboardEventData _ Sdl.Pressed False (Sdl.Keysym Sdl.ScancodeA _ _)) -> movePlayer exchange West
        Sdl.KeyboardEvent (Sdl.KeyboardEventData _ Sdl.Pressed False (Sdl.Keysym Sdl.ScancodeS _ _)) -> movePlayer exchange South
        Sdl.KeyboardEvent (Sdl.KeyboardEventData _ Sdl.Pressed False (Sdl.Keysym Sdl.ScancodeD _ _)) -> movePlayer exchange East
        -}
        Sdl.KeyboardEvent (Sdl.KeyboardEventData _ _ _ (Sdl.Keysym Sdl.ScancodeEscape _ _)) -> liftIO pushQuitEvent
        -- Sdl.KeyboardEvent (Sdl.KeyboardEventData _ _ _ (Sdl.Keysym Sdl.ScancodeSpace _ _)) -> do
        --     writeChan mixerChan "bell"
        Sdl.QuitEvent -> liftIO $ putMVar logicEndMVar ()
        _ -> return ()

renderThread :: MSampleVar GameState -> Sdl.Renderer -> Assets -> IO ()
renderThread gameStateSV renderer assets = forever $ do
    -- Wait for the game state to change
    gameState <- readSV gameStateSV

    -- Render that game state
    renderGame renderer assets gameState

networkThread :: IO ()
networkThread = forever $ do
    threadDelay 1000000
    
-- Thread-safe. Also, we keep retrying on failures (every 250ms) if the event queue is full.
-- SDL documents that the event is copied into their queue and that we can immediately dispose of our pointer after our function call.
pushQuitEvent :: IO ()
pushQuitEvent = alloca $ \ptr -> do
    poke ptr (Raw.QuitEvent 256 0)
    void $ iterateWhile (< 0) $ do
        code <- Raw.pushEvent ptr
        threadDelay 250000
        return code