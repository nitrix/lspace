module Ship where

import Coordinate
import Control.Lens
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Object
import Linear (V2(..))
import Linear.Affine (Point(..))

data Ship = MkShip
    { _shipMass         :: Integer
    , _shipVelocity     :: Integer
    , _shipDirection    :: Direction
    , _shipCoordinate   :: Coordinate
    , _shipRecentChunks :: S.Set Coordinate -- ^ List of recently mutated chunks
    , _shipChunks       :: M.Map Coordinate (V.Vector [Object])
    }

chunkCoord c = let (x, y) = tupleCoord c in coordinate (x `div` 10) (y `div` 10)
tupleCoord c = let (P (V2 x y)) = getCoordinate c in (x, y)
chunkIdx c = let (x, y) = tupleCoord c in fromInteger $ (y `mod` 10) * 10 + (x `mod` 10)
        
insert :: Coordinate -> Object -> Ship -> Ship
insert coord object ship = ship
    { _shipChunks = M.insertWith mutChunk (chunkCoord coord) newChunk (_shipChunks ship)
    , _shipMass = (_shipMass ship) + 1
    , _shipRecentChunks = S.insert (chunkCoord coord) (_shipRecentChunks ship)
    }
    where
        mutChunk new old = old V.// [(chunkIdx coord, object : (old V.! chunkIdx coord))]
        newChunk = V.replicate 100 [] V.// [(chunkIdx coord, [object])]
        
lookupChunk :: Coordinate -> Ship -> Maybe (V.Vector [Object])
lookupChunk coord ship = M.lookup (chunkCoord coord) (_shipChunks ship)

lookupCell :: Coordinate -> Ship -> [Object]
lookupCell coord ship = fromMaybe [] $ (V.! chunkIdx coord) <$> lookupChunk coord ship

{-
adjust :: Bool
adjust = undefined

range :: Bool
range = undefined
-}