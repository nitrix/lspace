{-# LANGUAGE OverloadedStrings #-}

-- Could be needed to interrupt a thread stuck on a foreign call
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ffi-chap.html#interruptible-foreign-calls
-- Might be in a critical section and leave C in some corrupted state.

module Kawaii.Core where

import Control.Concurrent
import Control.Concurrent.MSampleVar
import Control.Monad.State
import Foreign.Storable
import Foreign.Marshal.Alloc
-- import Control.Monad.Trans
import qualified SDL as Sdl
import qualified SDL.Raw.Event as Raw
import qualified SDL.Raw.Types as Raw

type Event = Sdl.EventPayload -- TODO: temporary
newtype Game a = Game { unwrapGame :: StateT GameState IO a }
data GameState = Gamestate
    { gsTest :: Int
    }
    
runGame :: Game () -> IO ()
runGame _undefined = runInBoundThread $ do
    Sdl.initializeAll
    
    -- SDL window and renderer
    window <- Sdl.createWindow "Lonesome Space" Sdl.defaultWindow
    renderer <- Sdl.createRenderer window (-1) Sdl.defaultRenderer
    
    -- Thread communication
    eventChan   <- newChan
    gameStateSV <- newEmptySV
    
    -- Threads
    logicThreadId   <- forkOS (logicThread eventChan gameStateSV)
    renderThreadId  <- forkOS (renderThread gameStateSV)
    networkThreadId <- forkOS (networkThread eventChan)
    timerThreadId   <- forkOS (timerThread eventChan)
    
    -- Event handling
    fix $ \loop -> do
        event <- Sdl.eventPayload <$> Sdl.waitEvent
        writeChan eventChan event
        unless (event == Sdl.QuitEvent) loop

    -- TODO: This is disgusting cleanup. It doesn't give time for the logicThreadId to save
    -- or networkThreadId to close the network connection properly.
    -- We should be coordinating with them and waiting for their termination.
    killThread logicThreadId
    killThread renderThreadId
    killThread networkThreadId
    killThread timerThreadId
    
    -- Cleanup
    Sdl.quit
    
logicThread :: Chan Event -> MSampleVar GameState -> IO ()
logicThread eventChan gameStateSV = fix $ \loop -> do
    event <- readChan eventChan
    case event of
        Sdl.KeyboardEvent (Sdl.KeyboardEventData _ _ _ (Sdl.Keysym Sdl.ScancodeEscape _ _)) -> pushQuitEvent
        Sdl.QuitEvent -> return ()
        _ -> do
            putStrLn $ takeWhile (/=' ') (show event)
            loop

renderThread :: MSampleVar GameState -> IO ()
renderThread _ = forever $ do
    threadDelay 1000000
    
networkThread :: Chan Event -> IO ()
networkThread _ = forever $ do
    threadDelay 1000000
    
timerThread :: Chan Event -> IO ()
timerThread _ = forever $ do
    threadDelay 1000000

-- Thread-safe, will keep retrying on failures if the event queue is full.
-- SDL documents that the event is copied into their queue and we can dispose of our pointer immediately after.
pushQuitEvent :: IO ()
pushQuitEvent = alloca $ \ptr -> do
    poke ptr (Raw.QuitEvent 256 0)
    fix $ \loop -> do
        code <- Raw.pushEvent ptr
        when (code < 0) $ do
            threadDelay 250000
            loop