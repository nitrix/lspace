{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Coordinate
    ( Coordinate(getCoordinate) -- TODO: ewww
    , Direction(..)
    , coordinate
    , coordinateMove
    , coordinates
    , coordinatesMove
    , coordinateX
    , coordinateY
    , defaultCoordinate
    , Metric
    ) where

import Control.Lens (Lens', lens, (%~), (&), view, set, (.~))
import Data.Aeson as J
import qualified Data.Text as T
import Linear (V2(V2), _x, _y)
import Linear.Affine (Point(P))
import Prelude

-- This is to eventually ease the transition to an infinite world if needed
type Metric = Int

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

instance ToJSON Coordinate where
    toJSON c = String . T.pack . show $ getCoordinate c

instance FromJSON Coordinate where
    parseJSON (J.String s) = return $ Coordinate $ read $ T.unpack s
    parseJSON _ = error "Unable to parse JSON for Coordinate"

newtype Coordinate = Coordinate { getCoordinate :: Point V2 Metric } deriving (Eq, Ord, Show, Read)

-- Lenses
coordinateX :: Lens' Coordinate Metric
coordinateY :: Lens' Coordinate Metric
coordinates :: Lens' Coordinate (Metric, Metric)
coordinateX = lens (view _x . getCoordinate) (\s z -> Coordinate $ getCoordinate s & _x .~ z)
coordinateY = lens (view _y . getCoordinate) (\s z -> Coordinate $ getCoordinate s & _y .~ z)
coordinates = lens (\c -> (view coordinateX c, view coordinateY c)) (\s c -> s & set coordinateX (fst c)
                                                                               & set coordinateY (snd c))
-- | Simplified Coordinate constructor
coordinate :: Metric -> Metric -> Coordinate
coordinate x y = Coordinate $ P $ V2 x y

-- | Compute a new coordinate relative to an existing coordinate in a given direction
coordinateMove :: Direction -> Coordinate -> Coordinate
coordinateMove North = coordinateY %~ subtract 1
coordinateMove South = coordinateY %~ (+1)
coordinateMove West  = coordinateX %~ subtract 1
coordinateMove East  = coordinateX %~ (+1)

coordinatesMove :: Direction -> (Metric, Metric) -> (Metric, Metric)
coordinatesMove direction (x, y) = view coordinates $ coordinateMove direction (coordinate x y)

-- | Center point
defaultCoordinate :: Coordinate
defaultCoordinate = coordinate 0 0
