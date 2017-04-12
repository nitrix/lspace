{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kawaii.Renderer where

import Control.Monad.Reader
import qualified SDL as Sdl

import Kawaii.Game

data RenderContext c = RenderContext
    { rcRenderer   :: Sdl.Renderer
    , rcSceneState :: GameState c
    }
newtype Renderer c a = Renderer { unwrapRenderer :: ReaderT (RenderContext c) IO a } deriving (Functor, Applicative, Monad)