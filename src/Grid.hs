module Grid where

import Prelude hiding (lookup)
import qualified Data.List as L
import Data.Function

type Region k = (k, k, k, k)

data Point k v = MkPoint k k [v] deriving (Show)

data Quad k v = MkQuad
    { qTopLeft     :: Grid k v
    , qTopRight    :: Grid k v
    , qBottomLeft  :: Grid k v
    , qBottomRight :: Grid k v
    } deriving (Show)

data Leaf k v = MkLeaf (Region k)
                       {-# UNPACK #-} !(Point k v)
                deriving (Show)

data Node k v = MkNode (Region k)
                       {-# UNPACK #-} !(Quad k v)
                deriving (Show)

data Grid k v = GridLeaf {-# UNPACK #-} !(Leaf k v)
              | GridNode {-# UNPACK #-} !(Node k v)
              | GridLeafEmpty (Region k)
              | GridEmpty
              deriving (Show)

empty :: Grid k v
empty = GridEmpty

emptyQuad :: Integral k => Region k -> Quad k v
emptyQuad r@(lx, ly, hx, hy) = MkQuad
    { qTopLeft     = GridLeafEmpty (lx, ly, cx, cy)
    , qTopRight    = GridLeafEmpty (cx, ly, hx, cy)
    , qBottomLeft  = GridLeafEmpty (lx, cy, cx, hy)
    , qBottomRight = GridLeafEmpty (cx, cy, hx, hy)
    }
    where
        (cx, cy) = centerRegion r

promoteLeafToNode :: Integral k => Leaf k v -> Node k v
promoteLeafToNode (MkLeaf r@(lx, ly, hx, hy) p@(MkPoint x y _))
    | x <= cx && y <= cy = MkNode r $ (emptyQuad r) { qTopLeft     = newGridLeaf (lx, ly, cx, cy) }
    | x >  cx && y <= cy = MkNode r $ (emptyQuad r) { qTopRight    = newGridLeaf (cx, ly, hx, cy) }
    | x <= cx && y >  cy = MkNode r $ (emptyQuad r) { qBottomLeft  = newGridLeaf (lx, cy, cx, hy) }
    | x >  cx && y >  cy = MkNode r $ (emptyQuad r) { qBottomRight = newGridLeaf (cx, cy, hx, hy) }
    | otherwise = error "Grid: This point should not have been inside of this leaf in the first place"
    where
        newGridLeaf nr = (GridLeaf $ MkLeaf nr p)
        (cx, cy) = centerRegion r

insert :: Integral k => k -> k -> v -> Grid k v -> Grid k v
insert x y v GridEmpty = GridLeaf $ MkLeaf (negate npof, negate npof, npof, npof) (MkPoint x y [v])
    where
        npof = nearestPowerOfFour (max (abs x) (abs y))
insert x y v (GridLeaf leaf@(MkLeaf r (MkPoint px py pvs))) =
    if x == px && y == py
    then GridLeaf $ MkLeaf r $ MkPoint px py (v:pvs)
    else GridNode (promoteLeafToNode leaf) & insert x y v
insert x y v (GridLeafEmpty r@(lx, ly, hx, hy))
    | x >= lx && x <= hx && y >= ly && y <= hy = GridLeaf newLeaf
    | otherwise = insert x y v $ GridEmpty
    where
        newLeaf = MkLeaf r $ MkPoint x y [v]
insert x y v (GridNode (MkNode r@(lx, ly, hx, hy) quad))
    | x >  hx || y >  hy = insert x y v $ GridNode $ MkNode (lx*2, ly*2, hx*2, hy*2) $ MkQuad
        { qTopLeft     = GridNode $ MkNode (lx*2, ly*2, cx,     cy) $ (emptyQuad (lx*2, ly*2, cx,     cy)) { qBottomRight = qTopLeft     quad }
        , qTopRight    = GridNode $ MkNode (cx,   ly*2, hx*2,   cy) $ (emptyQuad (cx,   ly*2, hx*2,   cy)) { qBottomLeft  = qTopRight    quad }
        , qBottomLeft  = GridNode $ MkNode (lx*2,   cy, cx,   hy*2) $ (emptyQuad (lx*2,   cy, cx,   hy*2)) { qTopRight    = qBottomLeft  quad }
        , qBottomRight = GridNode $ MkNode (cx,     cy, hx*2, hy*2) $ (emptyQuad (cx,     cy, hx*2, hy*2)) { qTopLeft     = qBottomRight quad }

        }
    | x <= cx && y <= cy = GridNode $ MkNode r $ quad { qTopLeft     = insert x y v $ qTopLeft     quad }
    | x >  cx && y <= cy = GridNode $ MkNode r $ quad { qTopRight    = insert x y v $ qTopRight    quad }
    | x <= cx && y >  cy = GridNode $ MkNode r $ quad { qBottomLeft  = insert x y v $ qBottomLeft  quad }
    | x >  cx && y >  cy = GridNode $ MkNode r $ quad { qBottomRight = insert x y v $ qBottomRight quad }
    | otherwise = error "Grid: The data structure implementation isn't partitioning space properly"
    where
        (cx, cy) = centerRegion r
        
delete :: (Integral k, Eq v) => k -> k -> v -> Grid k v -> Grid k v
delete x y v g@(GridEmpty) = g
delete x y v g@(GridLeafEmpty _) = g
delete x y v g@(GridLeaf (MkLeaf r (MkPoint px py pvs)))
    | x == px && y == py = if null newValues then GridLeafEmpty r else GridLeaf $ MkLeaf r $ MkPoint px py newValues
    | otherwise = g
    where
        newValues = L.delete v pvs
delete x y v g@(GridNode (MkNode r quad))
    | x <= cx && y <= cy = cleanupGrid $ GridNode $ MkNode r $ quad { qTopLeft     = cleanupGrid $ delete x y v $ qTopLeft     quad }
    | x >  cx && y <= cy = cleanupGrid $ GridNode $ MkNode r $ quad { qTopRight    = cleanupGrid $ delete x y v $ qTopRight    quad }
    | x <= cx && y >  cy = cleanupGrid $ GridNode $ MkNode r $ quad { qBottomLeft  = cleanupGrid $ delete x y v $ qBottomLeft  quad }
    | x >  cx && y >  cy = cleanupGrid $ GridNode $ MkNode r $ quad { qBottomRight = cleanupGrid $ delete x y v $ qBottomRight quad }
    | otherwise = g
    where
        (cx, cy) = centerRegion r
        cleanupGrid :: Grid k v -> Grid k v
        cleanupGrid g@(GridLeaf _) = g
        cleanupGrid g@(GridEmpty) = g
        cleanupGrid g@(GridLeafEmpty _) = g
        cleanupGrid g@(GridNode (MkNode r quad))
            | isEmpty g = GridLeafEmpty r
            | otherwise = g

lookup :: Integral k => k -> k -> Grid k v -> [v]            
lookup x y GridEmpty = []
lookup x y (GridLeafEmpty _) = []
lookup x y (GridLeaf (MkLeaf r (MkPoint px py pvs)))
    | x == px && y == py = pvs
    | otherwise = []
lookup x y (GridNode (MkNode r quad))
    | x <= cx && y <= cy = lookup x y $ qTopLeft     quad
    | x >  cx && y <= cy = lookup x y $ qTopRight    quad
    | x <= cx && y >  cy = lookup x y $ qBottomLeft  quad
    | x >  cx && y >  cy = lookup x y $ qBottomRight quad
    | otherwise = []
    where
        (cx, cy) = centerRegion r

range :: Integral k => Region k -> Grid k v -> [v]
range tr GridEmpty = []
range tr (GridLeafEmpty _) = []
range tr@(lx, ly, hx, hy) (GridLeaf (MkLeaf r (MkPoint px py pvs)))
    | px >= lx && py >= ly && px <= hx && py <= hy = pvs
    | otherwise = []
range tr (GridNode (MkNode r quad)) =
    if overlap tr r
    then range tr (qTopLeft quad) ++ range tr (qTopRight quad) ++ range tr (qBottomLeft quad) ++ range tr (qBottomRight quad)
    where
        overlap (alx, aly, ahx, ahy) (blx, bly, bhx, bhy) = blx < ahx && alx < bhx && bly < ahy && aly < bhy

-- Doesn't check recursively, internal function only
isEmpty (GridNode (MkNode r quad)) = check (qTopLeft quad) && check (qTopRight quad) && check (qBottomLeft quad) && check (qBottomRight quad)
    where
        check GridEmpty = True
        check (GridLeaf _) = False
        check (GridLeafEmpty _) = True
        check (GridNode _) = False
isEmpty _ = False

centerRegion :: Integral k => Region k -> (k, k)
centerRegion (lx, ly, hx, hy) = (cx, cy)
    where
        dx = hx - lx
        dy = hy - ly
        cx = lx + dx `div` 2
        cy = ly + dy `div` 2

nearestPowerOfFour :: Integral k => k -> k
nearestPowerOfFour n = head $ dropWhile (<n) $ iterate (4*) 1
