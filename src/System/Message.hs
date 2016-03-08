module System.Message where

import Coordinate (Direction)

data Message = MkMessage
    { msgType :: MessageType
    , msgFrom :: Integer      -- TODO: circular dependency ObjectId
    , msgTo   :: Integer      -- TODO: circular dependency ObjectId
    }

data MessageType = ProximityMsg Direction
                 | MovedMsg Direction
                 | SteppedOnMsg
                 | UnknownMsg