-- Could be needed to interrupt a thread stuck on a foreign call
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ffi-chap.html#interruptible-foreign-calls
-- Might be in a critical section and leave C in some corrupted state.
{-# LANGUAGE RecordWildCards #-}

module Kawaii.Core
    ( App(..)
    , Mode(..)
    , runApp
    ) where

import Control.Concurrent
import Control.Concurrent.MSampleVar
import Control.Exception
import Control.Monad.Loops
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Map as M
import Foreign.Storable
import Foreign.Marshal.Alloc
import qualified SDL as Sdl
import qualified SDL.Mixer as Mix
import qualified SDL.Raw.Event as Raw
import qualified SDL.Raw.Types as Raw

-- newtype Game a = Game (StateT GameState IO a)
data GameState -- = AppState

data Mode = Fullscreen | Windowed Int Int
data App = App
    { appTitle  :: String
    , appMode   :: Mode
    }

-- This is meant to be used preferably with the RecordWildCards extension
data Exchange = Exchange
    { eventChan    :: Chan Sdl.EventPayload
    , appStateSV   :: MSampleVar GameState
    , mixerChan    :: Chan String
    , timerChan    :: Chan ()
    , logicEndMVar :: MVar ()
    , hRenderer    :: Sdl.Renderer
    , audioAssets  :: M.Map String Mix.Chunk
    }

runApp :: App -> IO ()
runApp app = runInBoundThread $ do
    -- We're going to need SDL
    Sdl.initializeAll
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

    -- TODO: Load music assets by wav extension and store them in map where the key is the filename
    {-
    bell <- Mix.load "bell.wav"
    let audioChunkAssets = M.empty
                         & M.insert "bell" bell
    -}
    
    -- Thread communication
    exchange@(Exchange {..}) <- Exchange
        <$> newChan       -- Event channel (SDL events to be processed by the logic thread)
        <*> newEmptySV    -- Game state sampling variable (contains a snapshot of the game state to render)
        <*> newChan       -- Mixer channel (how we request things to be played)
        <*> newChan       -- Timer channel (how we request things to happen in the future)
        <*> newEmptyMVar  -- Logic thread end signal
        <*> pure renderer -- Renderer
        <*> pure M.empty  -- Audio assets
    
    -- Threads
    logicThreadId   <- forkOS (logicThread exchange)
    renderThreadId  <- forkOS (renderThread exchange)
    networkThreadId <- forkOS (networkThread exchange)
    timerThreadId   <- forkOS (timerThread exchange)
    mixerThreadId   <- forkOS (mixerThread exchange)

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
    killThread timerThreadId
    killThread mixerThreadId

    -- Cleanup
    -- Mix.free bell
    Mix.closeAudio
    Sdl.quit

logicThread :: Exchange -> IO ()
logicThread (Exchange {..}) = forever $ do
    event <- readChan eventChan
    case event of
        Sdl.KeyboardEvent (Sdl.KeyboardEventData _ _ _ (Sdl.Keysym Sdl.ScancodeEscape _ _)) -> pushQuitEvent
        -- Sdl.KeyboardEvent (Sdl.KeyboardEventData _ _ _ (Sdl.Keysym Sdl.ScancodeSpace _ _)) -> do
        --     writeChan mixerChan "bell"
        Sdl.QuitEvent -> putMVar logicEndMVar ()
        _ -> do
            putStrLn $ takeWhile (/=' ') (show event)

renderThread :: Exchange -> IO ()
renderThread (Exchange {..}) = forever $ do
    Sdl.rendererDrawColor hRenderer Sdl.$= Sdl.V4 0 0 0 255
    Sdl.clear hRenderer
    Sdl.present hRenderer
    threadDelay 1000000

networkThread :: Exchange -> IO ()
networkThread _ = forever $ do
    threadDelay 1000000

timerThread :: Exchange -> IO ()
timerThread _ = forever $ do
    threadDelay 1000000

mixerThread :: Exchange -> IO ()
mixerThread (Exchange {..}) = forever $ do
    key <- readChan mixerChan
    case M.lookup key audioAssets of
        Just chunk -> do
            -- SDL yields an exception if we try to play more sounds simultanously than we have channels available.
            -- We ignore the incident when it happens. This will result in some sounds not playing in rare noisy situations.
            void $ tryJust (\e -> case e of Sdl.SDLCallFailed _ _ _ -> Just undefined; _ -> Nothing) (Mix.play chunk)
            -- Mix.playMusic {-Mix.Once-} music
        Nothing -> return ()
    
-- Thread-safe. Also, we keep retrying on failures (every 250ms) if the event queue is full.
-- SDL documents that the event is copied into their queue and that we can immediately dispose of our pointer after our function call.
pushQuitEvent :: IO ()
pushQuitEvent = alloca $ \ptr -> do
    poke ptr (Raw.QuitEvent 256 0)
    void $ iterateWhile (< 0) $ do
        code <- Raw.pushEvent ptr
        threadDelay 250000
        return code