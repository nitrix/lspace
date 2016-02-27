module System.Message where

import Coordinate (Direction)

data Message = MkMessage
    { msgType :: MessageType
    -- , msgFrom :: Integer -- TOOD: ObjectId
    }

data MessageType = ProximityMsg Direction
                 | MovedMsg Direction
                 | SteppedOnMsg
                 | UnknownMsg
