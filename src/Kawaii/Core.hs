{-# LANGUAGE OverloadedStrings #-}

-- Could be needed to interrupt a thread stuck on a foreign call
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ffi-chap.html#interruptible-foreign-calls
-- Might be in a critical section and leave C in some corrupted state.

module Kawaii.Core where

import Control.Concurrent
import Control.Concurrent.MSampleVar
import Control.Monad.State
-- import Control.Monad.Trans
import qualified SDL as Sdl

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
logicThread eventChan gameStateSV = forever $ do
    event <- readChan eventChan
    putStrLn $ takeWhile (/=' ') (show event)

renderThread :: MSampleVar GameState -> IO ()
renderThread _ = forever $ do
    threadDelay 1000000
    
networkThread :: Chan Event -> IO ()
networkThread _ = forever $ do
    threadDelay 1000000
    
timerThread :: Chan Event -> IO ()
timerThread _ = forever $ do
    threadDelay 1000000