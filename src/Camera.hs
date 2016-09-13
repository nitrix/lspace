module Camera
    ( Camera
    -- Lenses
    , cameraCoordinate
    , cameraPinned
    , cameraViewport
    , cameraWindowSize
    -- Functions
    -- , cameraAuto
    -- , cameraCenter
    , cameraMove
    -- , cameraTogglePinned
    , defaultCamera
    ) where

import Control.Lens
import Foreign.C.Types
import Linear (V2(V2), _x, _y)
import Linear.Affine (Point(P))

import Coordinate

-- | Top view camera onto the world (2 axis).
-- The user can move this freely or it might temporarily be locked on the player.
data Camera = MkCamera
    { _cameraCoordinate :: WorldCoordinate
    , _cameraPinned     :: Bool
    , _cameraViewport   :: V2 Int
    , _cameraWindowSize :: V2 CInt
    }

-- | Default camera at the default coordinate position
defaultCamera :: Camera
defaultCamera = MkCamera
    { _cameraCoordinate = defaultCoordinate
    , _cameraPinned     = False
    , _cameraViewport   = V2 0 0
    , _cameraWindowSize = V2 (CInt 0) (CInt 0)
    }

-- Lenses
cameraCoordinate :: Lens' Camera WorldCoordinate
cameraPinned     :: Lens' Camera Bool
cameraViewport   :: Lens' Camera (V2 Int)
cameraWindowSize :: Lens' Camera (V2 CInt)
cameraCoordinate = lens _cameraCoordinate (\s x -> s { _cameraCoordinate = x })
cameraPinned     = lens _cameraPinned     (\s x -> s { _cameraPinned     = x })
cameraViewport   = lens _cameraViewport   (\s x -> s { _cameraViewport   = x })
cameraWindowSize = lens _cameraWindowSize (\s x -> s { _cameraWindowSize = x })

-- | Move the camera in a specified Direction
cameraMove :: Direction -> Camera -> Camera
cameraMove North = cameraCoordinate . coordinateY %~ subtract 1
cameraMove South = cameraCoordinate . coordinateY %~ (+1)
cameraMove West  = cameraCoordinate . coordinateX %~ subtract 1
cameraMove East  = cameraCoordinate . coordinateX %~ (+1)

-- TODO: needs a serious refactoring
{-
cameraAuto :: WorldCoordinate -> Camera -> Camera
cameraAuto coord c = if c ^. cameraPinned then cameraCenter coord c else cameraBound coord c

cameraBound :: WorldCoordinate -> Camera -> Camera
cameraBound coord c = let (x, y) = view coordinates coord in c &~ do
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

cameraCenter :: WorldCoordinate -> Camera -> Camera
cameraCenter coord c = c &~ do
    cameraCoordinate . coordinateX .= view coordinateX coord - (fromIntegral $ view (cameraViewport . _x) c `div` 2)
    cameraCoordinate . coordinateY .= view coordinateY coord - (fromIntegral $ view (cameraViewport . _y) c `div` 2)

cameraTogglePinned :: WorldCoordinate -> Camera -> Camera
cameraTogglePinned coord c = c & cameraPinned %~ not & cameraAuto coord
-}