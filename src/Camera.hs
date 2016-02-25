module Camera where

import Control.Lens
import Coordinate

data Camera = MkCamera { _cameraCoordinate :: Coordinate }

defaultCamera :: Camera
defaultCamera = MkCamera { _cameraCoordinate = defaultCoordinate }

cameraCoordinate :: Lens' Camera Coordinate
cameraCoordinate f s = (\x -> s { _cameraCoordinate = x }) <$> (f $ _cameraCoordinate s )

cameraMove :: Direction -> Camera -> Camera
cameraMove UpDirection    = cameraCoordinate . coordinateY %~ subtract 1
cameraMove DownDirection  = cameraCoordinate . coordinateY %~ (+1)
cameraMove LeftDirection  = cameraCoordinate . coordinateX %~ subtract 1
cameraMove RightDirection = cameraCoordinate . coordinateX %~ (+1)
