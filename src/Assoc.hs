module Assoc
    ( Assoc
    , Assoc.fromList
    , Assoc.lookup
    , Assoc.lookupR
--    , Assoc.adjust
    , Assoc.adjustR
    )
where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
    
-- :: A.Assoc Coordinate ObjectId

data Assoc a b = MkAssoc (M.Map a (S.Set b)) (M.Map b (S.Set a))

lookup :: Ord a => a -> Assoc a b -> S.Set b
lookup k (MkAssoc left _) = fromMaybe S.empty $ M.lookup k left

lookupR :: Ord b => b -> Assoc a b -> S.Set a
lookupR k (MkAssoc _ right) = fromMaybe S.empty $ M.lookup k right

fromList :: (Ord a, Ord b) => [(a, b)] -> Assoc a b
fromList xs = MkAssoc
    (foldr innerInsert M.empty xs)
    (foldr innerInsert M.empty $ innerSwap <$> xs)
    where
        innerInsert (a, b) m = M.insertWith S.union a (S.singleton b) m
        innerSwap (a, b) = (b, a) -- TODO: swap from Data.Tuple ?

{-
adjust :: (b -> b) -> a -> Assoc a b -> Assoc a b
adjust f k (MkAssoc left right) = MkAssoc (leftGo left) (right)
    where
        leftGo :: M.Map a (S.Set b) -> M.Map a (S.Set b)
        leftGo x = M.adjust () k x
-}

adjustR :: (Ord a, Ord b) => (a -> a) -> b -> Assoc a b -> Assoc a b
adjustR f k (MkAssoc left right) = MkAssoc goLeft goRight
    where
        goLeft = foldr (\c m -> M.insertWith S.union c (S.singleton k) m) leftClean newRightValues
        leftClean = foldr (M.adjust (S.delete k)) left oldRightValues
        goRight = M.adjust (const $ newRightValues) k right
        oldRightValues = fromMaybe S.empty $ M.lookup k right
        newRightValues = S.map f oldRightValues
