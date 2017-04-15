module Kawaii.Updater where

import Control.Monad.State
import Kawaii.Game

type Updater c r = StateT c Game (r c)

{-
newtype Updater c = Updater { unwrapUpdater :: StateT c Game (Result c ()) } deriving (Functor, Applicative, Monad)
instance MonadState c (Updater c) where
    get = Game $ state (\gs -> (gsCustomState gs, gs))
    put custom = Game $ state (\gs -> ((), gs { gsCustomState = custom }))
-}