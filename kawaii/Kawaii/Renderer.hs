{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kawaii.Renderer where

import Control.Monad.Reader
import qualified SDL as Sdl

import Kawaii.Game

data RenderContext = RenderContext
    { rcRenderer   :: Sdl.Renderer
    , rcSceneState :: GameState
    }
newtype Renderer a = Renderer { unwrapRenderer :: ReaderT RenderContext IO a } deriving (Functor, Applicative, Monad)

