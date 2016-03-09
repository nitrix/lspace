module System.Message where

import Coordinate (Direction)

data Message = ProximityMsg Direction
             | MovedMsg Direction
             | SteppedOnMsg
             | UnknownMsg
             deriving (Show)
