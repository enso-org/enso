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


lookupNative :: String -> Maybe Any
lookupNative = flip Map.lookup nativeCalls

nativeCalls :: Map String Any
nativeCalls = Map.fromList $ [

------------------
-- === List === --
------------------

      ("List.+",        unsafeCoerce (return .: (++)                  :: [Any] -> [Any] -> IO [Any]))
    , ("List.append",   unsafeCoerce (return .: (\l e -> l ++ [e])    :: [Any] -> Any -> IO [Any]))
    , ("List.prepend",  unsafeCoerce (return .: flip (:)              :: [Any] -> Any -> IO [Any]))
    , ("List.length",   unsafeCoerce (return .  length                :: [Any] -> IO Int))
    , ("List.reverse",  unsafeCoerce (return .  reverse               :: [Any] -> IO [Any]))
    , ("List.take",     unsafeCoerce (return .: flip take             :: [Any] -> Int -> IO [Any]))
    , ("List.drop",     unsafeCoerce (return .: flip drop             :: [Any] -> Int -> IO [Any]))
    , ("List.sort",     unsafeCoerce (return .  sort                  :: [Int] -> IO [Int]))

    , ("List.map",      unsafeCoerce (forM                            :: [Any] -> (Any -> IO Any) -> IO [Any]))
    , ("List.fold",     unsafeCoerce ((\l i f -> foldlM (flip f) i l) :: [Any] -> Any -> (Any -> Any -> IO Any) -> IO Any))
    , ("List.zip",      unsafeCoerce (flip zipWithM                   :: [Any] -> (Any -> Any -> IO Any) -> [Any] -> IO [Any]))
    , ("List.filter",   unsafeCoerce ((\l p -> filterM p l)           :: [Any] -> (Any -> IO Bool) -> IO [Any]))

-----------------
-- === Int === --
-----------------

    , ("Int.==",       unsafeCoerce (return .: (==) :: Int -> Int -> IO Bool))
    , ("Int./=",       unsafeCoerce (return .: (/=) :: Int -> Int -> IO Bool))
    , ("Int.<",        unsafeCoerce (return .: (<)  :: Int -> Int -> IO Bool))
    , ("Int.<=",       unsafeCoerce (return .: (<=) :: Int -> Int -> IO Bool))
    , ("Int.>",        unsafeCoerce (return .: (>)  :: Int -> Int -> IO Bool))
    , ("Int.>=",       unsafeCoerce (return .: (>=) :: Int -> Int -> IO Bool))
    , ("Int.min",      unsafeCoerce (return .: min  :: Int -> Int -> IO Int))
    , ("Int.max",      unsafeCoerce (return .: max  :: Int -> Int -> IO Int))

    , ("Int.+",        unsafeCoerce (return .: (+) :: Int -> Int -> IO Int))
    , ("Int.*",        unsafeCoerce (return .: (*) :: Int -> Int -> IO Int))
    , ("Int.-",        unsafeCoerce (return .: (-) :: Int -> Int -> IO Int))
    , ("Int./",        unsafeCoerce (return .: div :: Int -> Int -> IO Int))
    , ("Int.%",        unsafeCoerce (return .: mod :: Int -> Int -> IO Int))
    , ("Int.^",        unsafeCoerce (return .: (^) :: Int -> Int -> IO Int))

    , ("Int.negate",   unsafeCoerce (return . negate :: Int -> IO Int))
    , ("Int.abs",      unsafeCoerce (return . abs    :: Int -> IO Int))
    , ("Int.signum",   unsafeCoerce (return . signum :: Int -> IO Int))

    , ("Int.pred",     unsafeCoerce (return . pred :: Int -> IO Int))
    , ("Int.succ",     unsafeCoerce (return . succ :: Int -> IO Int))
    , ("Int.even",     unsafeCoerce (return . even :: Int -> IO Bool))
    , ("Int.odd",      unsafeCoerce (return . odd  :: Int -> IO Bool))

    , ("Int.gcd",      unsafeCoerce (return .: gcd :: Int -> Int -> IO Int))
    , ("Int.lcm",      unsafeCoerce (return .: lcm :: Int -> Int -> IO Int))

    , ("Int.times",    unsafeCoerce (return .: replicate  :: Int -> Any -> IO [Any]))
    , ("Int.upto",     unsafeCoerce (return .: enumFromTo :: Int -> Int -> IO [Int]))

    , ("Int.toDouble", unsafeCoerce (return . fromIntegral :: Int -> IO Double))
    , ("Int.toString", unsafeCoerce (return . show         :: Int -> IO String))

--------------------
-- === Double === --
--------------------

    , ("Double.==",       unsafeCoerce (return .: (==) :: Double -> Double -> IO Bool))
    , ("Double./=",       unsafeCoerce (return .: (/=) :: Double -> Double -> IO Bool))
    , ("Double.<",        unsafeCoerce (return .: (<)  :: Double -> Double -> IO Bool))
    , ("Double.<=",       unsafeCoerce (return .: (<=) :: Double -> Double -> IO Bool))
    , ("Double.>",        unsafeCoerce (return .: (>)  :: Double -> Double -> IO Bool))
    , ("Double.>=",       unsafeCoerce (return .: (>=) :: Double -> Double -> IO Bool))
    , ("Double.min",      unsafeCoerce (return .: min  :: Double -> Double -> IO Double))
    , ("Double.max",      unsafeCoerce (return .: max  :: Double -> Double -> IO Double))

    , ("Double.+",        unsafeCoerce (return .: (+)  :: Double -> Double -> IO Double))
    , ("Double.*",        unsafeCoerce (return .: (*)  :: Double -> Double -> IO Double))
    , ("Double.-",        unsafeCoerce (return .: (-)  :: Double -> Double -> IO Double))
    , ("Double./",        unsafeCoerce (return .: (/)  :: Double -> Double -> IO Double))
    , ("Double.**",       unsafeCoerce (return .: (**) :: Double -> Double -> IO Double))

    , ("Double.negate",   unsafeCoerce (return . negate :: Double -> IO Double))
    , ("Double.abs",      unsafeCoerce (return . abs    :: Double -> IO Double))
    , ("Double.signum",   unsafeCoerce (return . signum :: Double -> IO Double))

    , ("Double.round",    unsafeCoerce (return . round   :: Double -> IO Int))
    , ("Double.ceiling",  unsafeCoerce (return . ceiling :: Double -> IO Int))
    , ("Double.floor",    unsafeCoerce (return . floor   :: Double -> IO Int))

    , ("Double.exp",      unsafeCoerce (return . exp  :: Double -> IO Double))
    , ("Double.log",      unsafeCoerce (return . log  :: Double -> IO Double))
    , ("Double.sqrt",     unsafeCoerce (return . sqrt :: Double -> IO Double))

    , ("Double.sin",      unsafeCoerce (return . sin   :: Double -> IO Double))
    , ("Double.cos",      unsafeCoerce (return . cos   :: Double -> IO Double))
    , ("Double.tan",      unsafeCoerce (return . tan   :: Double -> IO Double))
    , ("Double.asin",     unsafeCoerce (return . asin  :: Double -> IO Double))
    , ("Double.acos",     unsafeCoerce (return . acos  :: Double -> IO Double))
    , ("Double.atan",     unsafeCoerce (return . atan  :: Double -> IO Double))
    , ("Double.sinh",     unsafeCoerce (return . sinh  :: Double -> IO Double))
    , ("Double.cosh",     unsafeCoerce (return . cosh  :: Double -> IO Double))
    , ("Double.tanh",     unsafeCoerce (return . tanh  :: Double -> IO Double))
    , ("Double.asinh",    unsafeCoerce (return . asinh :: Double -> IO Double))
    , ("Double.acosh",    unsafeCoerce (return . acosh :: Double -> IO Double))
    , ("Double.atanh",    unsafeCoerce (return . atanh :: Double -> IO Double))

    , ("Double.toString", unsafeCoerce (return .  show :: Double -> IO String))
    , ("Double.toStringFormat", unsafeCoerce (return .:. toStringFormat :: Double -> Int -> Int -> IO String))

------------------
-- === Bool === --
------------------

    , ("Bool.==",       unsafeCoerce (return .: (==) :: Bool -> Bool -> IO Bool))
    , ("Bool./=",       unsafeCoerce (return .: (/=) :: Bool -> Bool -> IO Bool))
    , ("Bool.<",        unsafeCoerce (return .: (<)  :: Bool -> Bool -> IO Bool))
    , ("Bool.<=",       unsafeCoerce (return .: (<=) :: Bool -> Bool -> IO Bool))
    , ("Bool.>",        unsafeCoerce (return .: (>)  :: Bool -> Bool -> IO Bool))
    , ("Bool.>=",       unsafeCoerce (return .: (>=) :: Bool -> Bool -> IO Bool))
    , ("Bool.min",      unsafeCoerce (return .: min  :: Bool -> Bool -> IO Bool))
    , ("Bool.max",      unsafeCoerce (return .: max  :: Bool -> Bool -> IO Bool))

    , ("Bool.&&",       unsafeCoerce (return .: (&&) :: Bool -> Bool -> IO Bool))
    , ("Bool.||",       unsafeCoerce (return .: (&&) :: Bool -> Bool -> IO Bool))
    , ("Bool.not",      unsafeCoerce (return . not   :: Bool -> IO Bool))

    , ("Bool.toString", unsafeCoerce (return .  show :: Bool -> IO String))

--------------------
-- === String === --
--------------------

    , ("String.==",       unsafeCoerce (return .: (==) :: String -> String -> IO Bool))
    , ("String./=",       unsafeCoerce (return .: (/=) :: String -> String -> IO Bool))
    , ("String.<",        unsafeCoerce (return .: (<)  :: String -> String -> IO Bool))
    , ("String.<=",       unsafeCoerce (return .: (<=) :: String -> String -> IO Bool))
    , ("String.>",        unsafeCoerce (return .: (>)  :: String -> String -> IO Bool))
    , ("String.>=",       unsafeCoerce (return .: (>=) :: String -> String -> IO Bool))
    , ("String.min",      unsafeCoerce (return .: min  :: String -> String -> IO String))
    , ("String.max",      unsafeCoerce (return .: max  :: String -> String -> IO String))

    , ("String.+",        unsafeCoerce (return .: (++)        :: String -> String -> IO String))
    , ("String.length",   unsafeCoerce (return .  length      :: String -> IO Int))
    , ("String.reverse",  unsafeCoerce (return .  reverse     :: String -> IO String))
    , ("String.take",     unsafeCoerce (return .: flip take   :: String -> Int -> IO String))
    , ("String.drop",     unsafeCoerce (return .: flip drop   :: String -> Int -> IO String))
    , ("String.words",    unsafeCoerce (return .  words       :: String -> IO [String]))
    , ("String.lines",    unsafeCoerce (return .  lines       :: String -> IO [String]))
    , ("String.join",     unsafeCoerce (return .: intercalate :: String -> [String] -> IO String))

    , ("String.toString", unsafeCoerce (return                :: String -> IO String))

-----------------
-- === Ref === --
-----------------

    , ("ref",             unsafeCoerce (newIORef  :: Any -> IO (IORef Any)))
    , ("Ref.modify",      unsafeCoerce modifyRef)
    , ("Ref.read",        unsafeCoerce (readIORef :: IORef Any -> IO Any))

------------------
-- === Misc === --
------------------

    , ("app",           unsafeCoerce ((\f a -> f a)           :: (Any -> IO Any) -> Any -> IO Any))
    , ("comp",          unsafeCoerce (return .: (<=<)         :: (Any -> IO Any) -> (Any -> IO Any) -> IO (Any -> IO Any)))
    , ("flip",          unsafeCoerce ((\f -> return $ flip f) :: (Any -> Any -> IO Any) -> IO (Any -> Any -> IO Any)))

    , ("empty",         unsafeCoerce (return []      :: IO [Any]))
    , ("singleton",     unsafeCoerce (return . (:[]) :: Any -> IO [Any]))

    , ("switch",        unsafeCoerce (return .:. (\x y z -> if x then y else z) :: Bool -> Any -> Any -> IO Any))

    , ("readFile",      unsafeCoerce (readFile :: String -> IO String))

    , ("mean",          unsafeCoerce (return . (uncurry (/) . foldr (\e (s, c) -> (e + s, c + 1)) (0, 0)) :: [Double] -> IO Double))
    , ("differences",   unsafeCoerce (return . (\l -> zipWith (-) (drop 1 l) l) :: [Int] -> IO [Int]))
    , ("histogram",     unsafeCoerce (return . map (\l -> (head l, length l)) . group . sort :: [Int] -> IO [(Int, Int)]))
    , ("primes",        unsafeCoerce (return . primes                                        :: Int -> IO [Int]))
    , ("pi",            unsafeCoerce (return   pi                                            :: IO Double))

----------------------
--- === Shapes === ---
----------------------

    , ("initTrans",                unsafeCoerce (return     def        :: IO Transformation))
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

    , ("point",                    unsafeCoerce (return .:  Point      :: Double -> Double -> IO Point))
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

    , ("Geometry.layer",           unsafeCoerce (return .:  Layer      :: Geometry -> [Transformation] -> IO Layer))
    , ("graphics",                 unsafeCoerce (return .   Graphics   :: [Layer]  -> IO Graphics))
    , ("Graphics.layers",          unsafeCoerce (return .   _graphics  :: Graphics -> IO [Layer]))

--------------------------
--- === Charts API === ---
--------------------------

    , ("sampleData",    unsafeCoerce (sampleData   :: (Double -> IO Double) -> Double -> Double -> Int -> IO [Point]))

    , ("axes",   unsafeCoerce      (return .::: axes   :: Material -> Double -> Double -> Double -> Double -> Double -> IO [Layer]))
    , ("grid",   unsafeCoerce      (return .::: grid   :: Material -> Double -> Double -> Double -> Double -> Double -> IO [Layer]))

    , ("scatterChart",   unsafeCoerce (return .::::  scatterChart   :: Material -> Figure -> Double -> Double -> Double -> Double -> Double -> [Point] -> IO Layer))

    , ("barChart",       unsafeCoerce (return .:::.  barChart       :: Material -> Double -> Double -> Double -> Double -> Double -> [Point] -> IO Layer)) -- broken
    , ("barChartLayers", unsafeCoerce (return .:::.  barChartLayers :: Material -> Double -> Double -> Double -> Double -> Double -> [Point] -> IO Graphics))

    , ("autoScatterChartInt",    unsafeCoerce (return .::. autoScatterChartInt    :: Material -> Material -> Figure -> Double -> [Int]    -> IO Graphics))
    , ("autoScatterChartDouble", unsafeCoerce (return .::. autoScatterChartDouble :: Material -> Material -> Figure -> Double -> [Double] -> IO Graphics))

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

------------------------------------- IMPLEMENTATIONS -------------------------------------

------------------------------------
-- === Double Implementations === --
------------------------------------

toStringFormat :: Double -> Int -> Int -> String
toStringFormat v w dec = let format = "%" <> show w <> "." <> show dec <> "f" in printf format v

---------------------------------
-- === Ref Implementations === --
---------------------------------

modifyRef :: IORef Any -> (Any -> IO Any) -> IO (IORef Any)
modifyRef ref f = do
    v  <- readIORef ref
    v' <- f v
    writeIORef ref v'
    return ref

----------------------------------
-- === Misc Implementations === --
----------------------------------

primes :: Int -> [Int]
primes count = take count primes' where
    primes'   = 2 : filter isPrime [3, 5..]
    isPrime n = not $ any (\p -> n `rem` p == 0) $ takeWhile (\p -> p * p <= n) primes'

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
