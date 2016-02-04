module Camera where

import Linear (V2(V2), V4(V4))
import Linear.Affine (Point(P))

data Camera = MkCamera { screenWidth :: Int
                       , screenHeight :: Int
                       , positionInWorld :: Point V2 Integer
                       , positionInChunk :: Point V2 Word
                       }

cameraMoveUp :: Camera -> Camera
cameraMoveUp = id

cameraMoveDown :: Camera -> Camera
cameraMoveDown = id

cameraMoveLeft :: Camera -> Camera
cameraMoveLeft = id
                    
cameraMoveRight :: Camera -> Camera
cameraMoveRight = id