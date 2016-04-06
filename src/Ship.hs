module Ship where

import Coordinate
import qualified Data.Map as M
import qualified Data.Vector as V
import Object

data Ship = MkShip
    { shipMass       :: Integer
    -- , shipVelocity   :: Integer
    -- , shipDirection  :: Direction
    -- , shipCoordinate :: Coordinate
    , shipChunks     :: M.Map Coordinate (V.Vector Object)
    }

{-
insert :: Bool
insert = undefined

adjust :: Bool
adjust = undefined

lookup :: Bool
lookup = undefined

range :: Bool
range = undefined
-}
