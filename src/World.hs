module World where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State as S
import Data.List

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
    -- When you're a structural object; try to attach to objects on your edges that are structural too
    -- otherwise, try to attach to structural objects "underneath" you.
    structural <- objStructural <$> gameReadLink objLink
    nearbyObjectLinks <- concat <$> mapM worldObjectsAtLocation (
        if structural
        then [ coordinateMove North coord
             , coordinateMove East coord
             , coordinateMove South coord
             , coordinateMove West coord
             ]
        else [coord]
        )

    nearbyUniqueRegionLinks <- nub <$> (fmap (view objRegion) . filter (objStructural) <$> mapM gameReadLink nearbyObjectLinks)
    
    let (worldX, worldY) = view coordinates coord

    -- TODO: temporary hack to avoid objects turning into ships when they move
    -- fakeR <- view objRegion <$> gameReadLink objLink
    -- let nearbyUniqueRegionLinks' = if fakeR /= invalidLink then nub (fakeR : nearbyUniqueRegionLinks) else nearbyUniqueRegionLinks
    let nearbyUniqueRegionLinks' = nearbyUniqueRegionLinks

    -- TODO: those are still being implemented
    case nearbyUniqueRegionLinks' of
        [] -> do
            let region = defaultRegion & regionCoordinate .~ coord & regionGrid %~ G.insert 0 0 objLink
            newRegionLink <- gameCreateLink region
            gameModifyLink objLink $ objRegion .~ newRegionLink
            gameModifyLink objLink $ objCoordinate .~ coordinate 0 0
            modify $ gameRegions %~ (newRegionLink:)
        r:[] -> do
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
    
    -- Delete object from region
    gameModifyLink regionLink $ regionGrid %~ G.delete x y objLink
    
    -- Read region
    region <- gameReadLink regionLink
    
    -- Flood fill segments
    let location = coordinate x y
    let grid     = view regionGrid region

    worldRegionFloodFill 1 (coordinateMove East  location) region (const $ return ())
    worldRegionFloodFill 2 (coordinateMove South location) region (const $ return ())
    worldRegionFloodFill 3 (coordinateMove West  location) region (const $ return ())
    worldRegionFloodFill 4 (coordinateMove North location) region (const $ return ())
    east  <- nub . map (view objFloodFill) <$> mapM gameReadLink (uncurry G.lookup (view coordinates (coordinateMove East  location)) grid)
    south <- nub . map (view objFloodFill) <$> mapM gameReadLink (uncurry G.lookup (view coordinates (coordinateMove South location)) grid)
    west  <- nub . map (view objFloodFill) <$> mapM gameReadLink (uncurry G.lookup (view coordinates (coordinateMove West  location)) grid)
    north <- nub . map (view objFloodFill) <$> mapM gameReadLink (uncurry G.lookup (view coordinates (coordinateMove North location)) grid)
    let everyDirection = east ++ south ++ west ++ north

    let changeObjToTheNewRegion newRegionLink thisObjLink = do
            (thisX, thisY) <- view (objCoordinate . coordinates) <$> gameReadLink thisObjLink
            gameModifyLink thisObjLink $ objRegion .~ newRegionLink
            gameModifyLink newRegionLink $ regionGrid %~ G.insert thisX thisY thisObjLink
            gameModifyLink regionLink $ regionGrid %~ G.delete thisX thisY thisObjLink

    let createShipFromIsolatedRegionInDirection direction = do
            newRegionLink <- gameCreateLink defaultRegion
            worldRegionFloodFill 0 (coordinateMove direction location) region (changeObjToTheNewRegion newRegionLink)
            gameModifyLink newRegionLink $ regionCoordinate .~ view regionCoordinate region
            modify $ gameRegions %~ (newRegionLink:)

    -- Ignore if we're removing the tip of something, it's always safe
    when (length everyDirection > 1) $ do
        -- Create new ships from isolated flood filled region segments
        when ((==1) . length . filter (==1) $ everyDirection) (createShipFromIsolatedRegionInDirection East)
        when ((==1) . length . filter (==2) $ everyDirection) (createShipFromIsolatedRegionInDirection South)
        when ((==1) . length . filter (==3) $ everyDirection) (createShipFromIsolatedRegionInDirection West)
        when ((==1) . length . filter (==4) $ everyDirection) (createShipFromIsolatedRegionInDirection North)
    
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

-- TODO: This should allow you to perform a higher-order transformation to be crazy useful
worldRegionFloodFill :: Int -> RegionCoordinate -> Region Object -> (Link Object -> Game ()) -> Game ()
worldRegionFloodFill marker coord region func = do
    structurallySound <- allStructurallySoundAt coord
    if structurallySound
    then do
        -- Flood fill cell
        forM_ (uncurry G.lookup (view coordinates coord) (view regionGrid region)) $ \objLink -> do
            func objLink
            gameModifyLink objLink $ objFloodFill .~ marker

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
            when (any ((/= marker) . view objFloodFill) things) $ do
                worldRegionFloodFill marker (coordinateMove direction coord) region func
        allStructurallySoundAt :: RegionCoordinate -> Game Bool
        allStructurallySoundAt loc = (liftM2 (&&) or (not . null)) <$> things
            where
                things = mapM
                         (liftM objStructural . gameReadLink)
                         (uncurry G.lookup (view coordinates loc) grid)
