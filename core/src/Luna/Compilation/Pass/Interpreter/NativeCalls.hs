{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Compilation.Pass.Interpreter.NativeCalls where


import           Prelude.Luna

import qualified Data.Map           as Map
import           Data.Map           (Map)
import           Control.Monad.Fix  (mfix)
import           Unsafe.Coerce      (unsafeCoerce)
import           GHC.Prim           (Any)
import           Data.List          (sort, group)
import           Data.IORef         (newIORef, IORef, writeIORef, readIORef)

import           Network.HTTP       (ResponseCode, simpleHTTP, getResponseCode, getRequest, urlEncode)
import           System.Environment (lookupEnv)
import           Text.Printf        (printf)

import           Graphics.API
import           Luna.Compilation.Pass.Interpreter.Charts


nativeCalls :: Map String Any
nativeCalls = Map.fromList $ [
      ("initTrans",                unsafeCoerce (return     def        :: IO Transformation))
    , ("Transformation.scale",     unsafeCoerce (return .:. scale      :: Transformation -> Double -> Double -> IO Transformation))
    , ("Transformation.translate", unsafeCoerce (return .:. translate  :: Transformation -> Double -> Double -> IO Transformation))
    , ("Transformation.rotate",    unsafeCoerce (return .:  rotate     :: Transformation -> Double ->           IO Transformation))
    , ("Transformation.reflect",   unsafeCoerce (return .   reflect    :: Transformation ->                     IO Transformation))

    , ("square",                   unsafeCoerce (return .   Square     :: Double ->           IO Figure))
    , ("rectangle",                unsafeCoerce (return .:  Rectangle  :: Double -> Double -> IO Figure))
    , ("circle",                   unsafeCoerce (return .   Circle     :: Double ->           IO Figure))

    , ("circleGeometry",           unsafeCoerce (return .:  circleToGeo    :: Double ->           Material -> IO Geometry))
    , ("squareGeometry",           unsafeCoerce (return .:  squareToGeo    :: Double ->           Material -> IO Geometry))
    , ("rectangleGeometry",        unsafeCoerce (return .:. rectangleToGeo :: Double -> Double -> Material -> IO Geometry))

    , ("point",                    unsafeCoerce (return .:  Point            :: Double -> Double -> IO Point))
    , ("toDoublePairList",         unsafeCoerce (return .   toDoublePairList :: [Point] -> IO [(Double, Double)]))

    , ("initAttributes",           unsafeCoerce (return     def        :: IO Attributes))
    , ("color",                    unsafeCoerce (return .:: SolidColor :: Double -> Double -> Double -> Double -> IO Material))

    , ("Figure.primitive",         unsafeCoerce (return .:. Primitive  :: Figure -> Point -> Attributes -> IO Primitive))

    , ("Primitive.shape",          unsafeCoerce (return .   Shape      :: Primitive      -> IO Shape))
    , ("Shape.merge",              unsafeCoerce (return .:  Merge      :: Shape -> Shape -> IO Shape))
    , ("Shape.subtract",           unsafeCoerce (return .:  Subtract   :: Shape -> Shape -> IO Shape))
    , ("Shape.intersect",          unsafeCoerce (return .:  Intersect  :: Shape -> Shape -> IO Shape))

    , ("Shape.surface",            unsafeCoerce (return .   ShapeSurface :: Shape -> IO Surface))

    , ("geoElem",                  unsafeCoerce (return .   GeoElem    :: [Surface]  -> IO GeoComponent))
    , ("geoGroup",                 unsafeCoerce (return .   GeoGroup   :: [Geometry] -> IO GeoComponent))
    , ("GeoComponent.geometry",    unsafeCoerce (return .:. (\c t m -> Geometry c t (Just m)) :: GeoComponent -> Transformation -> Material -> IO Geometry))

    , ("Geometry.layer",           unsafeCoerce (return .:  mkLayer            :: Geometry -> [Transformation] -> IO Layer))
    , ("graphics",                 unsafeCoerce (return .   Graphics           :: [Layer]  -> IO Graphics))
    , ("Graphics.layers",          unsafeCoerce (return .   _graphics          :: Graphics -> IO [Layer]))
    , ("Graphics.shift",           unsafeCoerce (return .:  flip shiftGraphics :: Graphics -> Point -> IO Graphics))

    , ("sampleData",    unsafeCoerce (sampleData   :: (Double -> IO Double) -> Double -> Double -> Int -> IO [Point]))

    , ("axes",        unsafeCoerce      (return .:::  axes        :: Material ->        Double -> Double -> Double -> Double -> Double -> IO [Layer]))
    , ("grid",        unsafeCoerce      (return .:::  grid        :: Material ->        Double -> Double -> Double -> Double -> Double -> IO [Layer]))
    , ("gridLabeled", unsafeCoerce      (return .:::. gridLabeled :: Material -> Int -> Double -> Double -> Double -> Double -> Double -> IO [Layer]))

    , ("scatterChart", unsafeCoerce (return .::::  scatterChart   :: Material -> Figure -> Double -> Double -> Double -> Double -> Double -> [Point] -> IO Layer))
    , ("barChart",     unsafeCoerce (return .:::.  barChartLayers :: Material ->           Double -> Double -> Double -> Double -> Double -> [Point] -> IO Graphics))

    , ("autoScatterChartInt",    unsafeCoerce (return .::: autoScatterChartInt    :: Material -> Material -> Figure -> Double -> Double -> [Int]    -> IO Graphics))
    , ("autoScatterChartDouble", unsafeCoerce (return .::: autoScatterChartDouble :: Material -> Material -> Figure -> Double -> Double -> [Double] -> IO Graphics))

------------------------
--- === IoT Demo === ---
------------------------

    , ("temperature",          unsafeCoerce (return    Temperature  :: IO Temperature))
    , ("fan",                  unsafeCoerce (return    Fan          :: IO Fan))
    , ("controlPanel",         unsafeCoerce (return    ControlPanel :: IO ControlPanel))

    , ("Temperature.inside",                unsafeCoerce (return .: tempInside           :: Temperature  -> Double -> IO Double))
    , ("Temperature.outside",               unsafeCoerce (return .: tempOutside          :: Temperature  -> Double -> IO Double))
    , ("ControlPanel.temperatureThreshold", unsafeCoerce (return .: temperatureThreshold :: ControlPanel -> Double -> IO Double))
    , ("ControlPanel.display",              unsafeCoerce (          displayLCD           :: ControlPanel -> String -> String -> IO String))
    , ("Fan.power",                         unsafeCoerce (          fanOnOff             :: Fan          -> Bool             -> IO String))

--------------------------
-- === Experimental === --
--------------------------

    , ("fix",           unsafeCoerce (mfix                                 :: (Any -> IO Any)                                    -> IO Any))
    , ("app1to2",       unsafeCoerce ((\f a     -> return $ f a)           :: (Any -> Any -> IO Any)        -> Any               -> IO (Any -> IO Any)))
    , ("app2to2",       unsafeCoerce ((\f a b   -> f a b)                  :: (Any -> Any -> IO Any)        -> Any -> Any        -> IO Any))
    , ("app1to3",       unsafeCoerce ((\f a     -> return $ f a)           :: (Any -> Any -> Any -> IO Any) -> Any               -> IO (Any -> Any -> IO Any)))
    , ("app2to3",       unsafeCoerce ((\f a b   -> return $ f a b)         :: (Any -> Any -> Any -> IO Any) -> Any -> Any        -> IO (Any -> IO Any)))
    , ("app3to3",       unsafeCoerce ((\f a b c -> f a b c)                :: (Any -> Any -> Any -> IO Any) -> Any -> Any -> Any -> IO Any))
    , ("cycle3",        unsafeCoerce ((\f -> return $ (\b c a -> f a b c)) :: (Any -> Any -> Any -> IO Any)                      -> IO (Any -> Any -> Any -> IO Any)))
    , ("comp2",         unsafeCoerce (return .: (<==<)                     :: (Any -> IO Any) -> (Any -> Any -> IO Any) -> IO (Any -> Any -> IO Any)))
    , ("comp2to2",      unsafeCoerce comp2to2)
    , ("comp3to2",      unsafeCoerce comp3to2)
    , ("retFun2",       unsafeCoerce retFun2)

    , ("testControls",  unsafeCoerce (return .:: testControls :: String -> Int -> Double -> Bool -> IO String))

    ]

------------------------
--- === IoT Demo === ---
------------------------

-- helpers

getCode :: String -> IO ResponseCode
getCode url = simpleHTTP req >>= getResponseCode
    where req = getRequest url
defautlLcdEndpoint = "http://192.168.2.222:8000/display"
defautlFanEndpoint = "http://192.168.2.222:8000/fan"

-- implementation

data Temperature  = Temperature
data ControlPanel = ControlPanel
data Fan          = Fan

tempInside, tempOutside :: Temperature -> Double -> Double
tempInside  = const id
tempOutside = const id

temperatureThreshold :: ControlPanel -> Double -> Double
temperatureThreshold = const id

displayLCD :: ControlPanel -> String -> String -> IO String
displayLCD _ first second = do
    lcdEndpointMay <- lookupEnv "LCD_ENDPOINT"
    let endpoint = fromMaybe defautlLcdEndpoint lcdEndpointMay
    (code, _, _) <- getCode $ endpoint <> "?first=" <> urlEncode first <> "&second=" <> urlEncode second
    return $ case code of
        2 -> first <> "\n" <> second
        _ -> "error"

fanOnOff :: Fan -> Bool -> IO String
fanOnOff _ state = do
    fanEndpointMay <- lookupEnv "FAN_ENDPOINT"
    let endpoint = fromMaybe defautlFanEndpoint fanEndpointMay
    (code, _, _) <- getCode $ endpoint <> "?state=" <> show (fromEnum state)
    return $ case code of
        2 -> if state then "On" else "Off"
        _ -> "error"

------------------------------------------
-- === Experimental Implementations === --
------------------------------------------

retFun2 :: (Any -> Any -> IO Any) -> IO (Any -> IO (Any -> IO Any))
retFun2 f = return $ \x -> do
    return $ f x

comp2to2 :: (Any -> Any -> IO Any) -> (Any -> Any -> IO Any) -> (Any -> Any -> IO Any) -> IO (Any -> Any -> IO Any)
comp2to2 f1 f2 f = return $ \x y -> do
    f1xy <- f1 x y
    f2xy <- f2 x y
    f f1xy f2xy

comp3to2 :: (Any -> Any -> IO Any) -> (Any -> Any -> IO Any) -> (Any -> Any -> IO Any) -> (Any -> Any -> Any -> IO Any) -> IO (Any -> Any -> IO Any)
comp3to2 f1 f2 f3 f = return $ \x y -> do
    f1xy <- f1 x y
    f2xy <- f2 x y
    f3xy <- f3 x y
    f f1xy f2xy f3xy

(<==<)   :: Monad m => (b -> m c) -> (a -> a1 -> m b) -> (a -> a1 -> m c)
g <==< f = \x y -> f x y >>= g


testControls :: String -> Int -> Double -> Bool -> String
testControls s i d b = s <> " " <> show i <> " " <> show d <> " " <> show b
