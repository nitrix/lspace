module System.Message where

import Coordinate (Direction)

data Message = MovedMsg Direction
             | RotateMsg
             
             | ProximityMsg Direction
             | SteppedOnMsg
             | UnknownMsg
             deriving (Show)
