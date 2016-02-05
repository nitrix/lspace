module Camera where

import Control.Lens
import Coordinate

data Camera = MkCamera { _cameraCoordinate :: Coordinate }

defaultCamera :: Camera
defaultCamera = MkCamera { _cameraCoordinate = defaultCoordinate }

cameraCoordinate :: Lens' Camera Coordinate
cameraCoordinate f s = (\x -> s { _cameraCoordinate = x }) <$> (f $ _cameraCoordinate s )

cameraMoveUp :: Camera -> Camera
cameraMoveUp = cameraCoordinate . coordinateY %~ (subtract 1)

cameraMoveDown :: Camera -> Camera
cameraMoveDown = cameraCoordinate . coordinateY %~ (+1)

cameraMoveLeft :: Camera -> Camera
cameraMoveLeft = cameraCoordinate . coordinateX %~ (subtract 1)
                    
cameraMoveRight :: Camera -> Camera
cameraMoveRight = cameraCoordinate . coordinateX %~ (+1)
