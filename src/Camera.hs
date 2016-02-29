module Camera
    ( Camera
    , cameraCoordinate
    , cameraMove
    , defaultCamera
    ) where

import Control.Lens
import Coordinate

-- | Top view camera onto the world (2 axis).
-- The user can move this freely or it might temporarily be locked on the player.
data Camera = MkCamera { _cameraCoordinate :: Coordinate }

-- | Default camera at the default coordinate position
defaultCamera :: Camera
defaultCamera = MkCamera { _cameraCoordinate = defaultCoordinate }

-- Lenses
cameraCoordinate :: Lens' Camera Coordinate
cameraCoordinate = lens _cameraCoordinate (\s x -> s { _cameraCoordinate = x })

-- | Move the camera in a specified Direction
cameraMove :: Direction -> Camera -> Camera
cameraMove UpDirection    = cameraCoordinate . coordinateY %~ subtract 1
cameraMove DownDirection  = cameraCoordinate . coordinateY %~ (+1)
cameraMove LeftDirection  = cameraCoordinate . coordinateX %~ subtract 1
cameraMove RightDirection = cameraCoordinate . coordinateX %~ (+1)
