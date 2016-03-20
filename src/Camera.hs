module Camera
    ( Camera
    , cameraCoordinate
    , cameraViewport
    , cameraMove
    , defaultCamera
    ) where

import Control.Lens
import Coordinate
import Linear (V2(V2))
import Foreign.C.Types

-- | Top view camera onto the world (2 axis).
-- The user can move this freely or it might temporarily be locked on the player.
data Camera = MkCamera
    { _cameraCoordinate :: Coordinate
    , _cameraViewport   :: V2 CInt
    } deriving Show

-- | Default camera at the default coordinate position
defaultCamera :: Camera
defaultCamera = MkCamera
    { _cameraCoordinate = defaultCoordinate
    , _cameraViewport   = V2 (CInt 0) (CInt 0)
    }

-- Lenses
cameraCoordinate :: Lens' Camera Coordinate
cameraCoordinate = lens _cameraCoordinate (\s x -> s { _cameraCoordinate = x })
cameraViewport :: Lens' Camera (V2 CInt)
cameraViewport = lens _cameraViewport (\s x -> s { _cameraViewport = x })

-- | Move the camera in a specified Direction
cameraMove :: Direction -> Camera -> Camera
cameraMove UpDirection    = cameraCoordinate . coordinateY %~ subtract 1
cameraMove DownDirection  = cameraCoordinate . coordinateY %~ (+1)
cameraMove LeftDirection  = cameraCoordinate . coordinateX %~ subtract 1
cameraMove RightDirection = cameraCoordinate . coordinateX %~ (+1)
