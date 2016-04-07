module Ship where

import Coordinate
import qualified Data.Map as M
import qualified Data.Vector as V
import Object
import SDL
import Linear (V2(..))
import Linear.Affine (Point(..))

data Ship = MkShip
    { shipMass       :: Integer
    -- , shipVelocity   :: Integer
    -- , shipDirection  :: Direction
    -- , shipCoordinate :: Coordinate
    , shipChunks     :: M.Map Coordinate (Texture, V.Vector Object)
    }

insert :: Coordinate -> (Texture, V.Vector Object) -> Ship -> Ship
insert c p = undefined
    where
        nc = let (P (V2 x y)) = getCoordinate c in coordinate (x `div` 10) (y `div` 10)

{-
adjust :: Bool
adjust = undefined

lookup :: Bool
lookup = undefined

range :: Bool
range = undefined
-}