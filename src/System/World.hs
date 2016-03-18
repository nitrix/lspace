module System.World where

import Control.Monad.State
import Game
import Object

sysWorldAddObjectAtPlayer :: Object -> State Game ()
sysWorldAddObjectAtPlayer obj = return ()
