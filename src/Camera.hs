module Camera
    ( Camera
    -- Lenses
    , cameraCoordinate
    , cameraPinned
    , cameraViewport
    -- Functions
    , cameraAuto
    , cameraCenter
    , cameraMove
    , cameraTogglePinned
    , defaultCamera
    ) where

import Control.Lens
import Linear (V2(V2), _x, _y)
import Linear.Affine (Point(P))
import Foreign.C.Types

import Types.Coordinate

-- | Top view camera onto the world (2 axis).
-- The user can move this freely or it might temporarily be locked on the player.
data Camera = MkCamera
    { _cameraCoordinate :: Coordinate
    , _cameraViewport   :: V2 CInt
    , _cameraPinned     :: Bool
    }

-- | Default camera at the default coordinate position
defaultCamera :: Camera
defaultCamera = MkCamera
    { _cameraCoordinate = defaultCoordinate
    , _cameraViewport   = V2 (CInt 0) (CInt 0)
    , _cameraPinned     = False
    }

-- Lenses
cameraCoordinate :: Lens' Camera Coordinate
cameraPinned     :: Lens' Camera Bool
cameraViewport   :: Lens' Camera (V2 CInt)
cameraCoordinate = lens _cameraCoordinate (\s x -> s { _cameraCoordinate = x })
cameraPinned     = lens _cameraPinned     (\s x -> s { _cameraPinned     = x })
cameraViewport   = lens _cameraViewport   (\s x -> s { _cameraViewport   = x })

-- | Move the camera in a specified Direction
cameraMove :: Direction -> Camera -> Camera
cameraMove North c = c & cameraCoordinate . coordinateY %~ subtract 1
cameraMove South c = c & cameraCoordinate . coordinateY %~ (+1)
cameraMove West  c = c & cameraCoordinate . coordinateX %~ subtract 1
cameraMove East  c = c & cameraCoordinate . coordinateX %~ (+1)

-- TODO: needs a serious refactoring
cameraAuto :: Coordinate -> Camera -> Camera
cameraAuto coord c = if c ^. cameraPinned then cameraCenter coord c else cameraBound coord c

cameraBound :: Coordinate -> Camera -> Camera
cameraBound coord c = let (P (V2 x y)) = getCoordinate coord in c &~ do
    cameraCoordinate .= coordinate (min minCameraX (x-padding)) (min minCameraY (y-padding))
    cameraCoordinate %= (\(P (V2 cx cy)) -> if x >= cx+maxCameraX-1-padding
                                            then coordinate (x-maxCameraX+1+padding) cy
                                            else coordinate cx cy) . getCoordinate
    cameraCoordinate %= (\(P (V2 cx cy)) -> if y >= cy+maxCameraY-1-padding
                                            then coordinate cx (y-maxCameraY+1+padding)
                                            else coordinate cx cy) . getCoordinate
    where
        padding    = min (maxCameraX `div` 4) (maxCameraY `div` 4)
        minCameraX = c ^. cameraCoordinate . coordinateX
        minCameraY = c ^. cameraCoordinate . coordinateY
        maxCameraX = fromIntegral $ c ^. cameraViewport . _x
        maxCameraY = fromIntegral $ c ^. cameraViewport . _y

-- TODO: needs a refactoring
cameraCenter :: Coordinate -> Camera -> Camera
cameraCenter coord c = c &~ do
    cameraCoordinate . coordinateX .= view coordinateX coord - (fromIntegral $ view (cameraViewport . _x) c `div` 2)
    cameraCoordinate . coordinateY .= view coordinateY coord - (fromIntegral $ view (cameraViewport . _y) c `div` 2)

cameraTogglePinned :: Coordinate -> Camera -> Camera
cameraTogglePinned coord c = c & cameraPinned %~ not & cameraAuto coord
