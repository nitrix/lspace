module World where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State as S
import Data.List

import Debug.Trace

import Coordinate
import Game
import qualified Grid as G
import Link
import Object
import Region

-- TODO: higher-order "atObject" would be nice
worldAtObjectAddObject :: Link Object -> Link Object -> Game ()
worldAtObjectAddObject targetLink whatLink = do
    worldCoord <- worldObjectLocation targetLink
    worldAddObject whatLink worldCoord

worldAddObject :: Link Object -> WorldCoordinate -> Game ()
worldAddObject objLink coord = do
    nearbyObjectLinks <- concat <$> mapM worldObjectsAtLocation
        [ coordinateMove North coord
        , coordinateMove East coord
        , coordinateMove South coord
        , coordinateMove West coord
        -- , coord
        ]

    nearbyUniqueRegionLinks <- nub <$> (fmap (view objRegion) . filter (objStructural) <$> mapM gameReadLink nearbyObjectLinks)
    
    let (worldX, worldY) = view coordinates coord

    -- TODO: temporary hack to avoid objects turning into ships when they move
    -- fakeR <- view objRegion <$> gameReadLink objLink
    -- let nearbyUniqueRegionLinks' = if fakeR /= invalidLink then nub (fakeR : nearbyUniqueRegionLinks) else nearbyUniqueRegionLinks
    let nearbyUniqueRegionLinks' = nearbyUniqueRegionLinks

    trace ("region links: " ++ show nearbyUniqueRegionLinks') $ do
    
    -- TODO: those are still being implemented
    case nearbyUniqueRegionLinks' of
        [] -> do
            trace "New region" $ do
            let region = defaultRegion & regionCoordinate .~ coord & regionGrid %~ G.insert 0 0 objLink
            newRegionLink <- gameCreateLink region
            gameModifyLink objLink $ objRegion .~ newRegionLink
            gameModifyLink objLink $ objCoordinate .~ coordinate 0 0
            modify $ gameRegions %~ (newRegionLink:)
        r:[] -> do
            trace ("Adding to region #" ++ show r) $ do
            region <- gameReadLink r
            let (innerX, innerY) = ( fromIntegral $ worldX - region ^. regionCoordinate . coordinateX
                                   , fromIntegral $ worldY - region ^. regionCoordinate . coordinateY
                                   )
            gameModifyLink objLink $ objRegion .~ r
            gameModifyLink objLink $ objCoordinate .~ (coordinate innerX innerY)
            gameModifyLink r $ regionGrid %~ G.insert innerX innerY objLink
            
            -- TODO: there's a lot of ship-to/from-world coordinate conversion; I think all this stuff should move to Coordinate
        _:_ -> do
            error "Unable to merge ships yet"
            -- TODO: worldMergeRegion + add our object to that
            -- trace ("bleh: " ++ show nearbyUniqueRegionLinks') $ do
            -- TODO: If our object is structural, and the ones around it too, then it's enough to connect to the ships and merge them.
            -- Otherwise, same as the [] case.

worldRemoveObject :: Link Object -> Game ()
worldRemoveObject objLink = do
    regionLink <- view objRegion                     <$> gameReadLink objLink
    (x, y)     <- view (objCoordinate . coordinates) <$> gameReadLink objLink
    region     <- gameReadLink regionLink
    
    -- TODO: Detect if we broke the region into two disconnected regions
    -- let location = coordinate x y
    -- (Walk only the edges that are perimeter related)
    -- walkA <- regionPerimeterCheck (coordinateMove East location)  region
    -- walkB <- regionPerimeterCheck (coordinateMove South location) region
    -- walkC <- regionPerimeterCheck (coordinateMove West location)  region
    -- walkD <- regionPerimeterCheck (coordinateMove North location) region
    -- (The ones that are false are isolated and needs to be marked as a ship)
    
    -- Delete object from region
    gameModifyLink regionLink $ regionGrid %~ G.delete x y objLink
    
    -- If the region is now empty, then there's no point in keeping it.
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

worldRegionFloodFill :: Int -> RegionCoordinate -> Region Object -> Game ()
worldRegionFloodFill marker coord region = do
    structurallySound <- allStructurallySoundAt coord
    if structurallySound
    then do
        -- Flood fill cell
        mapM_ (flip gameModifyLink (objFloodFill .~ marker)) (uncurry G.lookup (view coordinates coord) (view regionGrid region))

        -- Continue for cells not already flood filled
        continueWhenNotFilledAlready North
        continueWhenNotFilledAlready South
        continueWhenNotFilledAlready East
        continueWhenNotFilledAlready West
    else return ()
    where
        grid = view regionGrid region
        continueWhenNotFilledAlready :: Direction -> Game ()
        continueWhenNotFilledAlready direction = do
            things <- mapM gameReadLink $ uncurry G.lookup (view coordinates (coordinateMove direction coord)) grid
            when (any ((==0) . view objFloodFill) things) $ do
                worldRegionFloodFill marker (coordinateMove direction coord) region
        allStructurallySoundAt :: RegionCoordinate -> Game Bool
        allStructurallySoundAt loc = (liftM2 (&&) or (not . null)) <$> things
            where
                things = mapM
                         (liftM objStructural . gameReadLink)
                         (uncurry G.lookup (view coordinates loc) grid)
