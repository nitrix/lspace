{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coordinate
    ( Coordinate
    , Direction(..)
    , coordinate
    , coordinateMove
    , coordinates
    , coordinatesMove
    , coordinateX
    , coordinateY
    , defaultCoordinate
    
    , RegionCoordinate
    , WorldCoordinate
    , RelativeCoordinate
    ) where

import Control.Lens (Lens', lens, (%~), (&), view, set, (.~))
import Data.Aeson as J
import qualified Data.Text as T
import Linear (V2(V2), _x, _y)
import Linear.Affine (Point(P))
import Prelude

data Direction = North
               | East
               | South
               | West
               deriving (Show, Read, Bounded, Enum, Eq, Ord)

instance ToJSON Direction where
    toJSON = String . T.pack . show

instance FromJSON Direction where
    parseJSON (J.String s) = return $ read $ T.unpack s
    parseJSON _ = error "Unable to parse JSON for Direction"

instance (Num a, Show a) => ToJSON (Coordinate a) where
    toJSON c = String . T.pack . show $ getCoordinate c

instance (Num a, Read a) => FromJSON (Coordinate a) where
    parseJSON (J.String s) = return $ Coordinate $ read $ T.unpack s
    parseJSON _ = error "Unable to parse JSON for Coordinate"
 
newtype Coordinate a = Coordinate { getCoordinate :: Point V2 a } deriving (Eq, Ord, Show, Read)

type WorldCoordinate = Coordinate Integer
type RegionCoordinate = Coordinate Int
type RelativeCoordinate = Coordinate Int

-- Lenses
coordinateX :: Lens' (Coordinate a) a
coordinateY :: Lens' (Coordinate a) a
coordinates :: Lens' (Coordinate a) (a, a)
coordinateX = lens (view _x . getCoordinate) (\s z -> Coordinate $ getCoordinate s & _x .~ z)
coordinateY = lens (view _y . getCoordinate) (\s z -> Coordinate $ getCoordinate s & _y .~ z)
coordinates = lens (\c -> (view coordinateX c, view coordinateY c))
                   (\s c -> s & set coordinateX (fst c) & set coordinateY (snd c))

-- | Simplified Coordinate constructor
coordinate :: a -> a -> Coordinate a
coordinate x y = Coordinate $ P $ V2 x y

-- | Compute a new coordinate relative to an existing coordinate in a given direction
coordinateMove :: Num a => Direction -> Coordinate a -> Coordinate a
coordinateMove North = coordinateY %~ subtract 1
coordinateMove South = coordinateY %~ (+1)
coordinateMove West  = coordinateX %~ subtract 1
coordinateMove East  = coordinateX %~ (+1)

coordinatesMove :: Num a => Direction -> (a, a) -> (a, a)
coordinatesMove direction (x, y) = view coordinates $ coordinateMove direction (coordinate x y)

-- | Center point
defaultCoordinate :: Num a => Coordinate a
defaultCoordinate = coordinate 0 0