{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Kawaii.Stage
    ( Stage(Stage)
    , Updating
    , Rendering
    , Result(..)
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
type Updating c = UpdateM c Game (Result c)
type Rendering c = StateT c (ReaderT RenderContext IO) ()

newtype UpdateM s m a = Updating { runUpdateM :: StateT s m a } deriving (Functor, Applicative, Monad, MonadState s)
-- newtype UpdateM s a = UpdateM { unwrapUpdateM :: StateT (s, [Stage s], [Stage s]) Game a } deriving (Functor, Applicative)

{-
instance Monad (UpdateM s) where
    return a = UpdateM $ return a
    Skipped   >>= f = _
    Succeeded >>= f = return Succeeded
-}

{-
instance MonadState s (UpdateM s) where
    get = UpdateM $ get >>= return . (\(x,_,_) -> x)
    put x = UpdateM $ modify (\(_, seenStages, futureStages) -> (x, seenStages, futureStages))
-}

-- stageModify :: ([Stage s] -> [Stage s]) -> UpdateM s ()
-- stageModify f = UpdateM $ state (\(s, seenStages, futureStages) -> ((), (s, f stages)))

data Result c = Success
              | Skip
              | Switch (Stage c)
              | Bring (Stage c)
              | Destroy
              | Terminate