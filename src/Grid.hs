{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grid where

import Data.Foldable (foldl')
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Prelude hiding (lookup)
import qualified Data.List as L

type Region k = (k, k, k, k)
type ChunkCoord k = (k, k)
type Chunk v = V.Vector [v]
newtype Grid k v = MkGrid { runGrid :: M.Map (ChunkCoord k) (Chunk v) }

instance Show (Grid k v) where
    show = const "{Grid}"

empty :: Grid k v
empty = MkGrid M.empty

emptyChunk :: Chunk v
emptyChunk = V.replicate (chunkSize * chunkSize) []

chunkSize :: Integral k => k
chunkSize = 25

fromList :: Integral k => [(k, k, v)] -> Grid k v
fromList xs = foldl' (\g (x, y, v) -> insert x y v g) empty xs

coord :: Integral k => k -> k -> ChunkCoord k
coord x y = (x `div` chunkSize, y `div` chunkSize)

idx :: Integral k => k -> k -> Int
idx x y = fromIntegral $ iy * chunkSize + ix
    where
        ix = x `mod` chunkSize
        iy = y `mod` chunkSize

insert :: Integral k => k -> k -> v -> Grid k v -> Grid k v
insert x y v g = MkGrid $ M.insertWith (const insertedTo) (coord x y) (insertedTo emptyChunk) (runGrid g)
    where
        insertedTo o = V.unsafeUpd o [(idx x y, [v])]

delete :: (Integral k, Eq v) => k -> k -> v -> Grid k v -> Grid k v
delete x y v g = MkGrid $ M.update (\chunk -> Just $ V.modify go chunk) (coord x y) (runGrid g)
    where
        go vec = VM.modify vec (L.delete v) (idx x y)

lookup :: Integral k => k -> k -> Grid k v -> [v]
lookup x y g = fromMaybe [] $ (\chunk -> chunk V.! idx x y) <$> M.lookup (coord x y) (runGrid g)

range :: forall k v. Integral k => Region k -> Grid k v -> [(k, k, v)]
range (lx, ly, hx, hy) g =
    foldl
    (\acc c -> fromMaybe [] (triage . processChunk c <$> M.lookup c (runGrid g)) ++ acc)
    []
    chunkCoords
    where
        lowChunkCoord   = coord lx ly
        lowChunkCoordX  = fst lowChunkCoord
        lowChunkCoordY  = snd lowChunkCoord
        highChunkCoord  = coord hx hy
        highChunkCoordX = fst highChunkCoord
        highChunkCoordY = snd highChunkCoord
        chunkCoords     = [(x, y) | x <- [lowChunkCoordX..highChunkCoordX], y <- [lowChunkCoordY..highChunkCoordY]]
        triage :: [(k, k, v)] -> [(k, k, v)]
        triage = filter (\(x,y,_) -> x >= lx && x <= hx && y >= ly && y <= hy)
        processChunk :: ChunkCoord k -> Chunk v -> [(k, k, v)]
        processChunk c chunk = concat $ V.toList $ V.imap (\i v -> xy c i v) chunk
        xy :: ChunkCoord k -> Int -> [v] -> [(k, k, v)]
        xy (cx, cy) i vs = foldl' (\acc v -> (cx * chunkSize + ix, cy * chunkSize + iy, v) : acc) [] vs
            where
                ix = fromIntegral i `mod` chunkSize
                iy = (fromIntegral i - ix) `div` chunkSize
