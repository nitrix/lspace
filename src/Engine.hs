{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Engine where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.StateVar
import qualified Data.Text as T
import qualified SDL       as Sdl
import qualified SDL.Image as Img
import qualified SDL.TTF   as Ttf

newtype Engine a = Engine { getEngine :: StateT EngineState IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState EngineState)

data Window = Window Sdl.Window Sdl.Renderer
data WindowConfig = WindowFullscreen

data EngineState = EngineState
    { _esWindows   :: [Window]
    }
makeLenses ''EngineState

defaultEngineState :: EngineState
defaultEngineState = EngineState []

withEngine :: Engine () -> IO ()
withEngine game = do
    Sdl.initializeAll
    Img.initialize [Img.InitPNG]
    void Ttf.init

    Sdl.disableScreenSaver
    Sdl.cursorVisible $= False

    {-
    -- tileset  <- Img.loadTexture renderer "assets/tileset.png"
    -- font     <- Ttf.openFont "assets/terminus.ttf" 16
    -}

    -- Game
    finalState <- execStateT (getEngine game) defaultEngineState 

    -- Cleanup
    mapM_ (\(Window w r) -> Sdl.destroyWindow w >> Sdl.destroyRenderer r) (view esWindows finalState)
    -- Sdl.destroyTexture tileset
    -- Ttf.closeFont font
    Ttf.quit
    Img.quit
    Sdl.quit

engineCreateWindow :: String -> WindowConfig -> Engine ()
engineCreateWindow title windowConfig = do
    window <- case windowConfig of
                WindowFullscreen -> Sdl.createWindow (T.pack title) Sdl.defaultWindow { Sdl.windowMode = Sdl.FullscreenDesktop }

    renderer <- Sdl.createRenderer window (-1) Sdl.defaultRenderer

    Sdl.clear renderer
    Sdl.present renderer
    Sdl.showWindow window
    
    modify $ esWindows %~ (Window window renderer:)
