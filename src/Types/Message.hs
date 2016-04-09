module Types.Message where

import Types.Coordinate (Direction)

data Message = MovedMsg Direction
             | RotateMsg
             
             | ProximityMsg Direction
             | SteppedOnMsg
             | UnknownMsg
