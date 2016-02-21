module Assoc
    ( Assoc
    , Assoc.fromList
    , Assoc.lookup
    )
where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

data Assoc a b = MkAssoc (M.Map a (S.Set b)) (M.Map b a)

lookup :: Ord a => a -> Assoc a b -> S.Set b
lookup k (MkAssoc left _) = fromMaybe S.empty $ M.lookup k left

fromList :: (Ord a, Ord b) => [(a, b)] -> Assoc a b
fromList xs = MkAssoc
    (foldr innerInsert M.empty xs)
    (M.fromList $ innerSwap <$> xs)
    where
        innerInsert (a, b) m = M.insertWith S.union a (S.singleton b) m
        innerSwap (a, b) = (b, a) -- TODO: swap from Data.Tuple ?
