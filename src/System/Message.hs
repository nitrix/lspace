module System.Message where

import Coordinate (Direction)

data Message = InteractMsg
             | ProximityMsg
             | SteppedOnMsg
             | MovedMsg Direction
