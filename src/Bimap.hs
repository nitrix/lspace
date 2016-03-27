module Bimap
    ( Bimap
    , adjust
    , adjustR
    , empty
    , fromList
    , insert
    , lookup
    , lookupR
    , split
    )
where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Tuple
import Prelude (const, foldr, Ord, (<$>), ($))

data Bimap a b = MkBimap (M.Map a (S.Set b)) (M.Map b (S.Set a))

lookup :: (Ord a) => a -> Bimap a b -> S.Set b
lookup k (MkBimap left _) = fromMaybe S.empty $ M.lookup k left

lookupR :: (Ord b) => b -> Bimap a b -> S.Set a
lookupR k (MkBimap _ right) = fromMaybe S.empty $ M.lookup k right

fromList :: (Ord a, Ord b) => [(a, b)] -> Bimap a b
fromList xs = MkBimap
    (foldr innerInsert M.empty xs)
    (foldr innerInsert M.empty $ swap <$> xs)
    where
        innerInsert (a, b) m = M.insertWith S.union a (S.singleton b) m

adjust :: (Ord a, Ord b) => (b -> b) -> a -> Bimap a b -> Bimap a b
adjust f k (MkBimap left right) = MkBimap goLeft goRight
    where
        goRight = foldr (\c m -> M.insertWith S.union c (S.singleton k) m) rightClean newLeftValues
        rightClean = foldr (M.adjust (S.delete k)) right oldLeftValues
        goLeft = M.adjust (const $ newLeftValues) k left
        oldLeftValues = fromMaybe S.empty $ M.lookup k left
        newLeftValues = S.map f oldLeftValues

adjustR :: (Ord a, Ord b) => (a -> a) -> b -> Bimap a b -> Bimap a b
adjustR f k (MkBimap left right) = MkBimap goLeft goRight
    where
        goLeft = foldr (\c m -> M.insertWith S.union c (S.singleton k) m) leftClean newRightValues
        leftClean = foldr (M.adjust (S.delete k)) left oldRightValues
        goRight = M.adjust (const $ newRightValues) k right
        oldRightValues = fromMaybe S.empty $ M.lookup k right
        newRightValues = S.map f oldRightValues

empty :: Bimap a b
empty = MkBimap M.empty M.empty

insert :: (Ord a, Ord b) => a -> b -> Bimap a b -> Bimap a b
insert x y (MkBimap left right) = MkBimap newLeft newRight
    where
        newLeft  = M.insertWith S.union x (S.singleton y) left
        newRight = M.insertWith S.union y (S.singleton x) right

split :: (Ord a) => a -> Bimap a b -> (M.Map a (S.Set b), M.Map a (S.Set b))
split k (MkBimap left _) = M.split k left
