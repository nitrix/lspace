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
           deriving Show

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
    
    mVarEvent  <- newEmptyMVar
    mVarRedraw <- newEmptyMVar
    mVarLogs   <- newEmptyMVar
    mVarEnd    <- newEmptyMVar
    
    -- TODO: Networking thread
    -- TODO: Timers thread
    
    -- Events thread
    threadEvents <- forkIO $ forever $ do
        event <- takeMVar mVarEvent
        putMVar mVarLogs (show event)
        -- TODO: do something with events, like passing them to scenes and stuff
        -- engineState <- execStateT (getEngine game) defaultEngineState 
        putMVar mVarRedraw ()
    
    -- Rendering thread
    threadRender <- forkIO $ runInBoundThread $ forever $ do
        takeMVar mVarRedraw
        Sdl.clear renderer
        -- TODO: render view appRender app
        Sdl.present renderer
    
    -- Logging thread
    void $ forkIO $ forever $ do
        msg <- takeMVar mVarLogs
        if not . null $ msg
        then putStrLn msg
        else do
            putMVar mVarEnd ()

    -- Window (has to be in bounded main thread)
    fix $ \loop -> do
        putMVar mVarLogs "Getting events"
        events <- (:) <$> Sdl.waitEvent <*> Sdl.pollEvents
        putMVar mVarLogs "Putting events"
        shouldHalts <- forM (map Sdl.eventPayload events) $ \event -> do
            putMVar mVarEvent (convertEvent event)
            return (event == Sdl.QuitEvent)
        unless (or shouldHalts) loop
    
    -- Terminate all threads
    putMVar mVarLogs "Killing event thread"
    void $ forkIO $ killThread threadEvents
    putMVar mVarLogs "Killing render thread"
    void $ forkIO $ killThread threadRender
    -- The logger thread is a little special because we do not want to lose any logs
    putMVar mVarLogs "Killing logger thread"
    putMVar mVarLogs ""
    takeMVar mVarEnd
    putStrLn "Terminated"

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
