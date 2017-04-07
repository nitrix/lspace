module Kawaii.Ui
    ( Result(..)
    , Ui
    ) where

import qualified SDL as Sdl
import Kawaii.Game

data Result = Success
            | Skip
            | Switch Ui
            | Bring Ui
            | Destroy
            | Terminate

type Ui = Sdl.EventPayload -> Game Result