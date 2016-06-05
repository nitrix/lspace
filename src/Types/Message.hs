module Types.Message where

import Types.Coordinate (Direction)

data Message = MovedMsg Direction
             | RotatedMsg Direction
             | ProximityMsg Direction
             | SteppedOnMsg

             | UnknownMsg -- Is it really useful? Except maybe to force an update of the object?
                          -- I guess it also encourages people to pattern match `_` the remainder of messages for
                          -- future proof maintenance. 
             deriving (Show)
