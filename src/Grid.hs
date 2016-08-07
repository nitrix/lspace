{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Grid
( Grid
, empty
, toList
, fromList
, insert
, delete
, range
, lookup
, reverseLookup
)
where

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Foldable (foldl')
-- import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T
-- import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Prelude hiding (lookup)
import qualified Data.List as L

-- TODO: use lenses for natural and reversed transformations

type Region k = (k, k, k, k)
type ChunkCoord k = (k, k)
type Chunk v = V.Vector [v]
data Grid k v = MkGrid !(M.Map (ChunkCoord k) (Chunk v))
                       !(M.Map v (S.Set (k, k)))

instance (ToJSON k, ToJSON v, Integral k) => ToJSON (Grid k v) where
    toJSON g = Array
             $ V.fromList
             $ map (\(x, y, v) -> object ["x" .= x, "y" .= y, "v" .= v])
             $ toList g

instance (FromJSON k, FromJSON v, Show k, Integral k, Ord v, Show v) => FromJSON (Grid k v) where
    parseJSON (Array a) = return
                        $ fromList
                        $ catMaybes
                        $ map (\obj -> parseMaybe (\o -> do
                            x <- o .: "x"
                            y <- o .: "y"
                            v <- o .: "v"
                            return ((x, y, v) :: (k, k, v))
                          ) obj)
                        $ map (\x -> case x of Object o -> o; _ -> error "Non-object in Grid array json")
                        $ V.toList a
    parseJSON _ = error "Unable to parse Grid json"

instance Show (Grid k v) where
    show = const "{Grid}"

-- instance (Integral k, ToJSON k, ToJSON v) => Show (Grid k v) where
--    show g = T.unpack $ T.decodeUtf8 $ LB.toStrict $ encode g

empty :: Grid k v
empty = MkGrid M.empty M.empty

emptyChunk :: Chunk v
emptyChunk = V.replicate (chunkSize * chunkSize) []

chunkSize :: Integral k => k
chunkSize = 25

fromList :: (Integral k, Ord v) => [(k, k, v)] -> Grid k v
fromList xs = foldl' (\g (x, y, v) -> insert x y v g) empty xs

toList :: forall k v. Integral k => Grid k v -> [(k, k, v)]
toList (MkGrid natural _) = concatMap (\(chunkCoord, chunk) -> concatMap (\(i, vs) -> xy chunkCoord i vs) $ zip [0..] $ V.toList chunk) $ M.toList natural
    where
        xy :: ChunkCoord k -> Int -> [v] -> [(k, k, v)]
        xy (cx, cy) i vs = foldl' (\acc v -> (cx * chunkSize + ix, cy * chunkSize + iy, v) : acc) [] vs
            where
                ix = fromIntegral i `mod` chunkSize
                iy = (fromIntegral i - ix) `div` chunkSize

coord :: Integral k => k -> k -> ChunkCoord k
coord x y = (x `div` chunkSize, y `div` chunkSize)

idx :: Integral k => k -> k -> Int
idx x y = (fromIntegral $ iy * chunkSize + ix)
    where
        ix = x `mod` chunkSize
        iy = y `mod` chunkSize

insert :: (Integral k, Ord v) => k -> k -> v -> Grid k v -> Grid k v
insert x y v (MkGrid natural reversed) = MkGrid
    (M.insertWith (const insertedTo) (coord x y) (insertedTo emptyChunk) natural)
    (M.insertWith S.union v (S.singleton (x, y)) reversed)
    where
        insertedTo o = V.unsafeUpd o [(idx x y, [v])]

delete :: (Integral k, Ord v) => k -> k -> v -> Grid k v -> Grid k v
delete x y v (MkGrid natural reversed) = MkGrid
    (M.update (\chunk -> Just $ V.modify go chunk) (coord x y) natural)
    (M.update (\s -> if S.size s == 1 then Nothing else Just (S.delete (x, y) s)) v reversed)
    where
        go vec = VM.modify vec (L.delete v) (idx x y)

lookup :: Integral k => k -> k -> Grid k v -> [v]
lookup x y (MkGrid natural _) = fromMaybe [] $ (\chunk -> chunk V.! idx x y) <$> M.lookup (coord x y) natural

reverseLookup :: Ord v => v -> Grid k v -> Maybe (k, k)
reverseLookup v (MkGrid _ reversed) = head . S.toList <$> M.lookup v reversed

range :: forall k v. Integral k => Region k -> Grid k v -> [(k, k, v)]
range (lx, ly, hx, hy) (MkGrid natural _) =
    foldl
    (\acc c -> fromMaybe [] (triage . processChunk c <$> M.lookup c natural) ++ acc)
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
