{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

-- Aliases for convenience
type Updating c = UpdateM c ()
type Rendering c = StateT c (ReaderT RenderContext IO) ()

-- newtype Updating c = Updating { runUpdating :: StateT (c, [Stage c]) Game () }
newtype UpdateM s a = UpdateM { unwrapUpdateM :: StateT (s, [Stage s], [Stage s]) Game (Result s) } deriving (Functor, Applicative)

instance Monad (UpdateM s) where
    return a = UpdateM $ return a
    Skipped   >>= f = _
    Succeeded >>= f = return Succeeded

instance MonadState s (UpdateM s) where
    get = UpdateM $ get >>= return . (\(x,_,_) -> x)
    put x = UpdateM $ modify (\(_, seenStages, futureStages) -> (x, seenStages, futureStages))

stageModify :: ([Stage s] -> [Stage s]) -> UpdateM s ()
stageModify f = UpdateM $ state (\(s, seenStages, futureStages) -> ((), (s, f stages)))

data Result s = Skipped
              | Succeeded

skip :: UpdateM s (Result s)
skip = UpdateM $ return Skipped

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