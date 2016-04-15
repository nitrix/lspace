module Ship where

import Control.Lens
import Data.Maybe
import Data.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Linear (V2(..))
import Linear.Affine (Point(..))
import Types.Coordinate
import Types.Object

data Ship = MkShip
    { _shipMass         :: Integer
    -- Needs multiple directions and velocities
    -- , _shipVelocity     :: Integer
    -- , _shipDirection    :: Direction
    , _shipCoordinate   :: Coordinate
    , _shipChunks       :: M.Map Coordinate (Bool, V.Vector [Object])
    }

defaultShip = MkShip
    { _shipMass       = 0
    --, _shipVelocity   = 0
    --, _shipDirection  = South
    , _shipCoordinate   = coordinate 0 0
    , _shipChunks       = M.empty
    }

chunkCoord c = let (x, y) = tupleCoord c in coordinate (x `div` 10) (y `div` 10)
tupleCoord c = let (P (V2 x y)) = getCoordinate c in (x, y)
chunkIdx c = let (x, y) = tupleCoord c in fromInteger $ (y `mod` 10) * 10 + (x `mod` 10)
        
insert :: Coordinate -> Object -> Ship -> Ship
insert coord object ship = ship
    { _shipChunks = M.insertWith mutChunk (chunkCoord coord) newChunk (_shipChunks ship)
    , _shipMass = (_shipMass ship) + 1
    }
    where
        mutChunk new (ob, ov) = (True, ov V.// [(chunkIdx coord, object : (ov V.! chunkIdx coord))])
        newChunk = (True, V.replicate 100 [] V.// [(chunkIdx coord, [object])])
        
lookupChunk :: Coordinate -> Ship -> Maybe (Bool, V.Vector [Object])
lookupChunk coord ship = M.lookup (chunkCoord coord) (_shipChunks ship)

lookupCell :: Coordinate -> Ship -> [Object]
lookupCell coord ship = fromMaybe [] $ ((V.! chunkIdx coord) . snd) <$> lookupChunk coord ship

adjust :: Coordinate -> ([Object] -> [Object]) -> Ship -> Ship
adjust coord f ship = ship
    { _shipChunks = M.adjust (\(b, v) -> (True, v V.// [((chunkIdx coord), f (v V.! chunkIdx coord))])) (chunkCoord coord) (_shipChunks ship)
    }

empty :: Ship
empty = defaultShip

fromList :: [(Coordinate, Object)] -> Ship
fromList xs = foldl' (\m (c, o) -> insert c o m) empty xs

{-
range :: Bool
range = undefined
-}
