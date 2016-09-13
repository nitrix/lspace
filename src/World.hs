module World where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State as S
import Coordinate
import Data.List
import Data.Maybe
import Game
import qualified Grid as G
import Link
import Object
import Region

worldAtObjectAddObject :: Link Object -> Link Object -> Game ()
worldAtObjectAddObject targetLink whatLink = do
    return ()
    {-
    target <- gameReadLink targetLink
    region <- gameReadLink (view objRegion target)
    
    let grid = view regionGrid region
    case G.reverseLookup whatLink grid of
        Just (x, y) -> worldAddObject whatLink (coordinate x y) -- TODO: needs to be world coord, not region coord
        Nothing     -> return ()
    -}

worldAddObject :: Num a => Link Object -> WorldCoordinate -> Game ()
worldAddObject objLink coord = do
    nearbyObjectLinks <- concat <$> mapM worldObjectsAtLocation
        [ coordinateMove North coord
        , coordinateMove East coord
        , coordinateMove South coord
        , coordinateMove West coord
        -- TODO: , coord 
        ]

    nearbyUniqueRegionLinks <- nub <$> (fmap (view objRegion) <$> mapM gameReadLink nearbyObjectLinks)
    
    let (worldX, worldY) = view coordinates coord
    
    -- TODO: those are still being implemented
    case nearbyUniqueRegionLinks of
        [] -> do
            let region = defaultRegion & regionCoordinate .~ coord & regionGrid %~ G.insert 0 0 objLink
            newRegionLink <- gameCreateLink region
            gameModifyLink objLink $ objRegion .~ newRegionLink
            modify $ gameRegions %~ (newRegionLink:)
        r:[] -> do
            region <- gameReadLink r
            let (innerX, innerY) = ( fromIntegral $ worldX - region ^. regionCoordinate . coordinateX
                                   , fromIntegral $ worldY - region ^. regionCoordinate . coordinateY
                                   )
            gameModifyLink objLink $ objRegion .~ r
            gameModifyLink r $ regionGrid %~ G.insert innerX innerY objLink
            -- TODO: there's a lot of ship-to/from-world coordinate conversion; I think all this stuff should move to Coordinate
        _:_ -> do
            -- TODO: If our object is solid, then it's enough to connect to the ships and merge them.
            -- Otherwise, same as the [] case.
            return () -- TODO: worldMergeRegion + add our object to that
    
    return ()

worldRemoveObject :: Link Object -> Game ()
worldRemoveObject objLink = do
    regionLink <- view objRegion                     <$> gameReadLink objLink
    (x, y)     <- view (objCoordinate . coordinates) <$> gameReadLink objLink
    
    gameModifyLink regionLink $ regionGrid %~ G.delete x y objLink
    
    -- TODO: detect if the object removed was holding two different parts of the ship and should become ships of their own
    
    -- If the region is now empty, then no point in keeping it.
    region <- gameReadLink regionLink
    when (null $ G.toList $ view regionGrid region) $ do
        gameDestroyLink regionLink
        modify $ gameRegions %~ delete regionLink -- TODO: could be more efficient, O(n) when lot of regions

worldRotateObject :: Link Object -> Direction -> Game ()
worldRotateObject objLink direction = do
    gameModifyLink objLink $ objFacing .~ direction

worldObjectsAtLocation :: WorldCoordinate -> Game [Link Object]
worldObjectsAtLocation coord = do
    regionLinks <- gets (view gameRegions)
    regions     <- mapM gameReadLink regionLinks

    fmap concat <$> forM regions $ \s -> do
        let (innerX, innerY) = ( fromIntegral $ worldX - s ^. regionCoordinate . coordinateX
                               , fromIntegral $ worldY - s ^. regionCoordinate . coordinateY
                               )
        return $ G.lookup innerX innerY $ view regionGrid s
        
    where
        (worldX, worldY) = view coordinates coord

-- Unless Ship is broken or we're looking at the wrong region for our object,
-- it should always be able to get the coordinate of the object.
-- Thus, I decided to reflect this in the type and provide defaultCoordinate as an absolute emergency.
-- It simplifies code that uses worldObjectLocation a whole lot.
worldObjectLocation :: Link Object -> Game WorldCoordinate
worldObjectLocation objLink = do
    regionLink <- view objRegion <$> gameReadLink objLink
    region     <- gameReadLink regionLink
    coord      <- view objCoordinate <$> gameReadLink objLink
    
    let (x, y) = view coordinates coord
                                  
    return $ coordinate ((region ^. regionCoordinate . coordinateX) + (fromIntegral x))
                        ((region ^. regionCoordinate . coordinateY) + (fromIntegral y))
            
worldMoveObject :: Link Object -> Direction -> Game ()
worldMoveObject objLink direction = do
    -- Rotate the object
    worldRotateObject objLink direction
    
    -- Asking the region's grid about the current position of our object
    newLocation <- coordinateMove direction <$> worldObjectLocation objLink
    
    -- We're going to perform collision detection of native objects at the target location
    natives <- mapM gameReadLink =<< worldObjectsAtLocation newLocation
    
    -- Allow moving the object only when there's no collisions detected
    when (all (not . objSolid) natives) $ do
        regionLink <- view objRegion <$> gameReadLink objLink
        region     <- gameReadLink regionLink

        -- If the object is the only one in its region, then move the whole region, otherwise, move the object
        if (length (G.toList $ view regionGrid region) == 1)
        then 
            worldMoveRegion regionLink direction
        else do
            worldRemoveObject objLink
            worldAddObject objLink newLocation

worldMoveRegion :: Link (Region Object) -> Direction -> Game ()
worldMoveRegion regionLink direction = do
    -- TODO: collision check; maybe even triggering explosions or something?
    gameModifyLink regionLink $ regionCoordinate %~ coordinateMove direction

    -- TODO: when a ship moves, next to / on top of, another ship, should they merge / check docking ports are aligned?