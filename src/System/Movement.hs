module System.Movement where

-- import Control.Lens
import Control.Monad.State

import Types.Coordinate (Direction)
import Types.Object (ObjectId)
import Types.Game

sysMove :: Direction -> ObjectId -> State Game ()
sysMove _ _ = do
    -- player <- gets $ view gamePlayer
    return ()
