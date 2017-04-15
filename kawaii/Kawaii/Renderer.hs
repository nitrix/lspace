{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kawaii.Renderer where

import Control.Monad.Reader
import Control.Monad.State
import qualified SDL as Sdl

import Kawaii.Game

data RenderContext = RenderContext
    { rcRenderer  :: Sdl.Renderer
    , rcGameState :: GameState
    }
newtype Renderer c a = Renderer { unwrapRenderer :: StateT c (ReaderT RenderContext IO) a } deriving (Functor, Applicative, Monad)