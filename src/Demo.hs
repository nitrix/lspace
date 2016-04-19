{-# LANGUAGE TupleSections #-}

module Demo
    ( demoShips
    ) where

import qualified Ship as H
-- import qualified Data.Map as M
import qualified Data.Set as S
import Object.Box
import Object.Player
import Types.Coordinate
import Types.Object
import Types.World

demoShips :: WorldShips
demoShips = S.singleton $ H.fromList $
    [ (coordinate 0 0, boxObject defaultObject defaultBox)
    , (coordinate 1 1, boxObject defaultObject defaultBox)
    , (coordinate 2 2, playerObject defaultObject defaultPlayer)
    ] -- ++ [(coordinate x y,0) | x <- [10..200], y <- [10..200]]
