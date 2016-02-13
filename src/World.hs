module World where

import Data.Map
import Object

type World = Map Integer [Object]

defaultWorld :: World
defaultWorld = empty
