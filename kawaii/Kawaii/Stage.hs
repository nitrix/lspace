module Kawaii.Stage
    ( Stage(Stage)
    , Updating
    , Rendering
    , bring
    , destroy
    , skip
    , success
    , switch
    , terminate
    ) where

import Control.Monad.Reader
import Control.Monad.State
import qualified SDL as Sdl

import Kawaii.Event
import Kawaii.Game

data Stage c = Stage
    { stageUpdating  :: Event -> Updating c
    , stageRendering :: Rendering c
    }

data RenderContext = RenderContext
    { rcRenderer  :: Sdl.Renderer
    , rcGameState :: GameState
    }

type Updating c = StateT (c, [Stage c]) Game ()

type Rendering c = StateT c (ReaderT RenderContext IO) ()

skip :: Updating c
skip = undefined

success :: Updating c
success = undefined

terminate :: Updating c
terminate = undefined

bring :: Stage c -> Updating c
bring _stage = undefined

switch :: Stage c -> Updating c
switch _stage = undefined

destroy :: Updating c
destroy = undefined