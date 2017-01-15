{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Engine
    ( App(..)
    , Event(..)
    , Mode(..)
    , Result(..)
    , Scene(..)
    , runApp
    )
    where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MSampleVar
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.State
import Data.StateVar
import qualified Data.Text as T
import Linear
import Linear.Affine
import qualified SDL       as Sdl
import qualified SDL.Image as Img
import qualified SDL.TTF   as Ttf

data Event = EventUnknown
           | EventMousePosition Int Int
           | EventQuit
           deriving (Show, Eq)

newtype Engine a = Engine { _runEngine :: StateT AppState IO a } deriving (Functor, Applicative, Monad)

data Mode = Fullscreen | Windowed Int Int

data Scene = Scene
    { sceneUpdate :: Event -> Engine Result
    , sceneRender :: Engine ()
    }

data Result = Success
            | Skip
            | Switch Scene
            | Bring Scene
            | Destroy
            | Terminate

data App s = App    
    { appTitle  :: String
    , appMode   :: Mode
    , appScenes :: [Scene]
    }

data AppState 

runApp :: App s -> IO ()
runApp app = runInBoundThread $ do
    Sdl.initializeAll
    Img.initialize [Img.InitPNG]
    void Ttf.init

    -- This might be too arbitrary but so far fairly convenient defaults
    Sdl.disableScreenSaver
    Sdl.cursorVisible $= False

    {-
    -- tileset  <- Img.loadTexture renderer "assets/tileset.png"
    -- font     <- Ttf.openFont "assets/terminus.ttf" 16
    -}
    
    -- Creating window and renderer
    window <- let cw wc = Sdl.createWindow (T.pack $ appTitle app) wc in do
        case appMode app of
            Fullscreen   -> cw $ Sdl.defaultWindow { Sdl.windowMode = Sdl.FullscreenDesktop }
            Windowed w h -> cw $ Sdl.defaultWindow { Sdl.windowInitialSize = V2 (fromIntegral w) (fromIntegral h) }
    
    renderer <- liftIO $ runInBoundThread $ Sdl.createRenderer window (-1) Sdl.defaultRenderer
    
    -- Preparing the window and renderer
    Sdl.clear renderer
    Sdl.present renderer
    Sdl.showWindow window
    
    mVarEvent <- newEmptyMVar
    mVarLogs  <- newEmptyMVar
    svRedraw  <- newEmptySV
    
    -- TODO: Networking thread
    -- TODO: Timers thread
    
    -- Events thread
    eventThread <- async $ fix $ \loop -> do
        event <- takeMVar mVarEvent
        putMVar mVarLogs (show event)
        -- TODO: do something with events, like passing them to scenes and stuff
        -- engineState <- execStateT (getEngine game) defaultEngineState 
        writeSV svRedraw (event == EventQuit) -- Using a sample var (multiple write/overwrite, single read blocking)
        when (event /= EventQuit) loop
    
    -- Logging thread
    loggingThread <- async $ fix $ \loop -> do
        msg <- takeMVar mVarLogs
        when (not . null $ msg) $ do
            putStrLn msg
            loop

    -- Rendering thread
    renderThread <- async $ runInBoundThread $ fix $ \loop -> do
        stop <- readSV svRedraw -- Waiting here
        when (not stop) $ do
            Sdl.rendererDrawColor renderer $= (V4 0 0 0 0)
            Sdl.clear renderer
            -- TODO: render view appRender app
            Sdl.rendererDrawColor renderer $= (V4 255 0 0 0)
            Sdl.drawPoint renderer (P $ V2 100 100)
            Sdl.present renderer
            loop
    
    -- Window thread
    runInBoundThread $ fix $ \loop -> do
        events <- (:) <$> Sdl.waitEvent <*> Sdl.pollEvents
        shouldHalts <- forM (map Sdl.eventPayload events) $ \event -> do
            putMVar mVarEvent (convertEvent event)
            return (event == Sdl.QuitEvent)
        if (or shouldHalts) then do
            putMVar mVarEvent EventQuit
        else do
            loop
    
    -- Terminate all threads. The order matters because of possible MVar deadlocks.
    void $ waitCatch renderThread
    void $ waitCatch eventThread
    putMVar mVarLogs "" -- TODO: This kills the logger thread. It's dirty
    void $ waitCatch loggingThread

    -- Cleanup
    Sdl.destroyWindow window
    Sdl.destroyRenderer renderer
    -- Sdl.destroyTexture tileset
    -- Ttf.closeFont font
    Ttf.quit
    Img.quit
    Sdl.quit

convertEvent :: Sdl.EventPayload -> Event
convertEvent (Sdl.MouseMotionEvent e) = let (P (V2 x y)) = Sdl.mouseMotionEventPos e in EventMousePosition (fromIntegral x) (fromIntegral y)
convertEvent _ = EventUnknown
