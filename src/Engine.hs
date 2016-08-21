module Engine
    ( engineHandleEvent
    , engineHandleKeyboardEvent
    -- , engineInit
    ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State as S
import Data.List
import Data.Maybe
import Linear (V2(V2))
import SDL

import Camera
import Coordinate
import Environment
import Game
import qualified Grid as G
import Link
import Object
import Region
import Ui
import Ui.Menu

-- | This function takes care of all events in the engine and dispatches them to the appropriate handlers.
engineHandleEvent :: Event -> Game Bool
engineHandleEvent event = do
    case eventPayload event of
        KeyboardEvent d      -> engineHandleKeyboardEvent d
        WindowResizedEvent d -> engineHandleWindowResizedEvent d
        QuitEvent            -> return True
        _                    -> return False

engineHandleWindowResizedEvent :: WindowResizedEventData -> Game Bool
engineHandleWindowResizedEvent wred = do
    tileSize <- gameEnv envTileSize
    let V2 width height = windowResizedEventSize wred

    modify $ gameCamera . cameraWindowSize .~ V2 (fromIntegral width) (fromIntegral height)
    modify $ gameCamera . cameraViewport .~ V2
        (fromIntegral width `div` fromIntegral tileSize)
        (fromIntegral height `div` fromIntegral tileSize)

    return False

-- | This function handles keyboard events in the engine
engineHandleKeyboardEvent :: KeyboardEventData -> Game Bool
engineHandleKeyboardEvent ked = do
    -- Modifier keys
    case keycode of
        KeycodeLShift -> modify $ gameKeyShift .~ (keymotion == Pressed)    
        KeycodeRShift -> modify $ gameKeyShift .~ (keymotion == Pressed)    
        KeycodeLAlt   -> modify $ gameKeyAlt   .~ (keymotion == Pressed)    
        KeycodeRAlt   -> modify $ gameKeyAlt   .~ (keymotion == Pressed)    
        _             -> return ()

    -- Bare keys
    if (keymotion == Pressed) then do
        (newKeycode, shouldHalt) <- uiMenuInterceptKeycode keycode
        if shouldHalt
        then return True
        else engineHandleBareKeycode newKeycode
    else 
        return False -- $ scancode == ScancodeEscape
    where
        keymotion   = keyboardEventKeyMotion ked -- ^ Wether the key is being pressed or released
        keysym      = keyboardEventKeysym ked    -- ^ Key symbol information: keycode or scancode representation
        keycode     = keysymKeycode keysym       -- ^ Which character is received from the operating system
        -- scancode    = keysymScancode keysym      -- ^ Physical key location as it would be on a US QWERTY keyboard

engineHandleBareKeycode :: Keycode -> Game Bool
engineHandleBareKeycode keycode = do
    player <- gets $ view gamePlayer
    shift  <- gets $ view gameKeyShift
    case keycode of
        KeycodeW       -> if shift then engineRotateObject player North else engineMoveObject player North
        KeycodeS       -> if shift then engineRotateObject player South else engineMoveObject player South
        KeycodeA       -> if shift then engineRotateObject player West  else engineMoveObject player West
        KeycodeD       -> if shift then engineRotateObject player East  else engineMoveObject player East
        -- TODO: zoom levels with caching
        -- KeycodeKPPlus  -> modify $ gameCamera %~ cameraZoom (subtract 1)
        -- KeycodeKPMinus -> modify $ gameCamera %~ cameraZoom (+1)
        KeycodeUp      -> modify $ gameCamera %~ cameraMove North
        KeycodeDown    -> modify $ gameCamera %~ cameraMove South
        KeycodeRight   -> modify $ gameCamera %~ cameraMove East
        KeycodeLeft    -> modify $ gameCamera %~ cameraMove West
        -- TODO: toggling centered camera and/or camera following when moving near edges
        -- KeycodeY       -> modify $ id -- gameCamera %~ (fromMaybe id (cameraTogglePinned <$> sysWorldCoordObjectId world player)) -- TODO: eeeww
        KeycodeE       -> modify $ gameUi       %~ uiMenuSwitch UiMenuMain
        KeycodeEscape  -> modify $ gameUi       %~ uiMenuClear
        _              -> modify $ id
    return False

engineAddObject :: Link Object -> Coordinate -> Game ()
engineAddObject objLink coord = do
    nearbyObjectLinks <- concat <$> mapM engineObjectsAtLocation
        [ coordinateMove North coord
        , coordinateMove East coord
        , coordinateMove South coord
        , coordinateMove West coord
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
            let (innerX, innerY) = (worldX - region ^. regionCoordinate . coordinateX, worldY - region ^. regionCoordinate . coordinateY)
            gameModifyLink objLink $ objRegion .~ r
            gameModifyLink r $ regionGrid %~ G.insert innerX innerY objLink
            -- TODO: there's a lot of ship-to/from-world coordinate conversion; I think all this stuff should move to Coordinate
        _:_ -> do
            return () -- TODO: engineMergeRegion + add our object to that
    
    return ()

engineRemoveObject :: Link Object -> Game ()
engineRemoveObject objLink = do
    regionLink <- view objRegion <$> gameReadLink objLink
    gameModifyLink regionLink $ regionGrid %~ G.reverseDelete objLink
    
    -- If the region is now empty, then no point in keeping it.
    region <- gameReadLink regionLink
    when (null $ G.toList $ view regionGrid region) $ do
        gameDestroyLink regionLink
        modify $ gameRegions %~ delete regionLink -- TODO: could be more efficient, O(n) when lot of regions

engineRotateObject :: Link Object -> Direction -> Game ()
engineRotateObject objLink direction = do
    gameModifyLink objLink $ objFacing .~ direction

engineObjectsAtLocation :: Coordinate -> Game [Link Object]
engineObjectsAtLocation coord = do
    regionLinks <- gets (view gameRegions)
    regions     <- mapM gameReadLink regionLinks

    fmap concat <$> forM regions $ \s -> do
        let (innerX, innerY) = (worldX - s ^. regionCoordinate . coordinateX, worldY - s ^. regionCoordinate . coordinateY)
        return $ G.lookup innerX innerY $ view regionGrid s
        
    where
        (worldX, worldY) = view coordinates coord

-- Unless Ship is broken or we're looking at the wrong region for our object,
-- it should always be able to get the coordinate of the object.
-- Thus, I decided to reflect this in the type and provide defaultCoordinate as an absolute emergency.
-- It simplifies code that uses engineObjectLocation a whole lot.
engineObjectLocation :: Link Object -> Game Coordinate
engineObjectLocation objLink = do
    regionLink <- view objRegion <$> gameReadLink objLink
    region     <- gameReadLink regionLink
    
    let (x, y) = view coordinates $ fromMaybe defaultCoordinate
                                  $ uncurry coordinate <$> G.reverseLookup objLink (view regionGrid region)
                                  
    return $ coordinate (region ^. regionCoordinate . coordinateX + x)
                        (region ^. regionCoordinate . coordinateY + y)
            
engineMoveObject :: Link Object -> Direction -> Game ()
engineMoveObject objLink direction = do
    -- Rotate the object
    engineRotateObject objLink direction
    
    -- Asking the region's grid about the current position of our object
    newLocation <- coordinateMove direction <$> engineObjectLocation objLink
    
    -- We're going to perform collision detection of native objects at the target location
    natives <- mapM gameReadLink =<< engineObjectsAtLocation newLocation
    
    -- Allow moving the object only when there's no collisions detected
    when (all (not . objSolid) natives) $ do
        regionLink <- view objRegion <$> gameReadLink objLink
        region     <- gameReadLink regionLink

        -- If the object is the only one in its region, then move the whole region, otherwise, move the object
        if (length (G.toList $ view regionGrid region) == 1)
        then 
            engineMoveRegion regionLink direction
        else do
            engineRemoveObject objLink
            engineAddObject objLink newLocation

engineMoveRegion :: Link (Region Object) -> Direction -> Game ()
engineMoveRegion regionLink direction = do
    -- TODO: collision check; maybe even triggering explosions or something?
    gameModifyLink regionLink $ regionCoordinate %~ coordinateMove direction

    -- TODO: when a ship moves, next to / on top of, another ship, should they merge?
