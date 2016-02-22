module Assoc
    ( Assoc
    , Assoc.fromList
    , Assoc.lookup
    , Assoc.lookupR
    )
where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple

data Assoc a b = MkAssoc (M.Map a (S.Set b)) (M.Map b (S.Set a))

lookup :: Ord a => a -> Assoc a b -> S.Set b
lookup k (MkAssoc left _) = fromMaybe S.empty $ M.lookup k left

lookupR :: Ord b => b -> Assoc a b -> S.Set a
lookupR k (MkAssoc _ right) = fromMaybe S.empty $ M.lookup k right

fromList :: (Ord a, Ord b) => [(a, b)] -> Assoc a b
fromList xs = MkAssoc
    (foldr innerInsert M.empty xs)
    (foldr innerInsert M.empty $ swap <$> xs)
    where
        innerInsert (a, b) m = M.insertWith S.union a (S.singleton b) m