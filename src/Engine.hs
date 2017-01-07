{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Engine
    ( App
    , Event(..)
    , Mode(..)
    , Renderer
    , Result(..)
    , Scene
    , withEngine
    , engineCreateApp
    , engineRunApp
    )
    where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.StateVar
import qualified Data.Text as T
import qualified SDL       as Sdl
import qualified SDL.Image as Img
import qualified SDL.TTF   as Ttf

type Renderer = RenderM ()
newtype RenderM a = RenderM { runRenderM :: IO a } deriving (Functor, Applicative, Monad)
type Scene = Event -> Engine Result

data Event = EventUnknown deriving Show

data Result = Continue
            | Skip
            | Switch Scene
            | Bring Scene
            | Terminate

data App = App
    { _appWindow   :: Sdl.Window
    , _appRenderer :: Sdl.Renderer
    , _appEvents   :: MVar [Event]
    }
    
appWindow :: Lens' App Sdl.Window
appWindow = lens _appWindow (\s x -> s { _appWindow = x })
appRenderer :: Lens' App Sdl.Renderer
appRenderer = lens _appRenderer (\s x -> s { _appRenderer = x })
appEvents :: Lens' App (MVar [Event])
appEvents = lens _appEvents (\s x -> s { _appEvents = x })
-- makeLenses ''App

data EngineState = EngineState
    { _esApps :: [App]
    }

esApps :: Lens' EngineState [App]
esApps = lens _esApps (\s x -> s { _esApps = x })
-- makeLenses ''EngineState

newtype Engine a = Engine { getEngine :: StateT EngineState IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState EngineState)

data Mode = ModeFullscreen

defaultEngineState :: EngineState
defaultEngineState = EngineState
    { _esApps = []
    }

withEngine :: Engine () -> IO ()
withEngine game = runInBoundThread $ do
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

    -- Running the engine computation
    engineState <- execStateT (getEngine game) defaultEngineState 

    -- Cleanup
    forM_ (view esApps engineState) $ \app -> do
        Sdl.destroyWindow (view appWindow app)
        Sdl.destroyRenderer (view appRenderer app)
    -- Sdl.destroyTexture tileset
    -- Ttf.closeFont font
    Ttf.quit
    Img.quit
    Sdl.quit

engineCreateApp :: String -> Mode -> Engine App
engineCreateApp title mode = do
    window <- case mode of
                ModeFullscreen -> liftIO $ runInBoundThread $ Sdl.createWindow (T.pack title) Sdl.defaultWindow { Sdl.windowMode = Sdl.FullscreenDesktop }

    renderer <- liftIO $ runInBoundThread $ Sdl.createRenderer window (-1) Sdl.defaultRenderer

    Sdl.clear renderer
    Sdl.present renderer
    Sdl.showWindow window
    
    mvar <- liftIO newEmptyMVar
    let app = App window renderer mvar
    
    modify $ esApps %~ (app:)
    return app

engineRunApp :: App -> [Scene] -> Renderer -> Engine ()
engineRunApp app scenes render = do
    redraw <- liftIO newEmptyMVar
    
    -- Window events
    liftIO $ void $ forkIO $ runInBoundThread $ forever $ do
        events <- (:) <$> Sdl.waitEvent <*> Sdl.pollEvents
        putMVar mvarEvents [EventUnknown] -- events TODO: convert sdl events to engine events
    
    -- TODO: Network events
    -- TODO: Timer events
    
    liftIO $ void $ forkIO $ forever $ do
        events <- takeMVar mvarEvents
        print events
        -- TODO: do something with events, like passing them to scenes and stuff
        putMVar redraw ()
    
    liftIO $ void $ forkIO $ runInBoundThread $ forever $ do
        takeMVar redraw
        -- Sdl.clear sdlRenderer
        runRenderM render
        -- Sdl.present sdlRenderer
    
    return ()
    where
        mvarEvents = view appEvents app
        sdlRenderer = view appRenderer app