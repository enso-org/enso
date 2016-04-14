module Luna.Compilation.Pass.Interpreter.NativeCalls where


import           Prelude.Luna

import qualified Data.Map as Map
import           Data.Map (Map)
import           Unsafe.Coerce   (unsafeCoerce)
import           GHC.Prim        (Any)

nativeCalls :: Map String Any
nativeCalls = Map.fromList $
-----------------
-- === Int === --
-----------------
    [ ("Int.+",        unsafeCoerce (return .: (+) :: Int -> Int -> IO Int))
    , ("Int.*",        unsafeCoerce (return .: (*) :: Int -> Int -> IO Int))
    , ("Int.-",        unsafeCoerce (return .: (-) :: Int -> Int -> IO Int))
    , ("Int./",        unsafeCoerce (return .: div :: Int -> Int -> IO Int))
    , ("Int.%",        unsafeCoerce (return .: mod :: Int -> Int -> IO Int))
    , ("Int.^",        unsafeCoerce (return .: (^) :: Int -> Int -> IO Int))

    , ("Int.negate",   unsafeCoerce (return . negate :: Int -> IO Int))
    , ("Int.abs",      unsafeCoerce (return . abs    :: Int -> IO Int))
    , ("Int.signum",   unsafeCoerce (return . signum :: Int -> IO Int))
    , ("Int.pred",     unsafeCoerce (return . pred   :: Int -> IO Int))
    , ("Int.succ",     unsafeCoerce (return . succ   :: Int -> IO Int))
    , ("Int.even",     unsafeCoerce (return . even   :: Int -> IO Bool))
    , ("Int.odd",      unsafeCoerce (return . odd    :: Int -> IO Bool))

    , ("Int.==",       unsafeCoerce (return .: (==) :: Int -> Int -> IO Bool))
    , ("Int./=",       unsafeCoerce (return .: (/=) :: Int -> Int -> IO Bool))
    , ("Int.<",        unsafeCoerce (return .: (<)  :: Int -> Int -> IO Bool))
    , ("Int.<=",       unsafeCoerce (return .: (<=) :: Int -> Int -> IO Bool))
    , ("Int.>",        unsafeCoerce (return .: (>)  :: Int -> Int -> IO Bool))
    , ("Int.>=",       unsafeCoerce (return .: (>=) :: Int -> Int -> IO Bool))

    , ("Int.min",      unsafeCoerce (return .: min :: Int -> Int -> IO Int))
    , ("Int.max",      unsafeCoerce (return .: max :: Int -> Int -> IO Int))
    , ("Int.gcd",      unsafeCoerce (return .: gcd :: Int -> Int -> IO Int))
    , ("Int.lcm",      unsafeCoerce (return .: lcm :: Int -> Int -> IO Int))

    , ("Int.toString", unsafeCoerce (return .  show         :: Int -> IO String))
    , ("Int.toDouble", unsafeCoerce (return .  fromIntegral :: Int -> IO Double))
    , ("Int.times",    unsafeCoerce (return .: replicate    :: Int -> Any -> IO [Any]))
    , ("Int.upto",     unsafeCoerce (return .: enumFromTo   :: Int -> Int -> IO [Int]))

--------------------
-- === Double === --
--------------------
    , ("Double.+",        unsafeCoerce (return .: (+)    :: Double -> Double -> IO Double))
    , ("Double.*",        unsafeCoerce (return .: (*)    :: Double -> Double -> IO Double))
    , ("Double.-",        unsafeCoerce (return .: (-)    :: Double -> Double -> IO Double))
    , ("Double./",        unsafeCoerce (return .: (/)    :: Double -> Double -> IO Double))
    , ("Double.**",       unsafeCoerce (return .: (**)   :: Double -> Double -> IO Double))
    , ("Double.negate",   unsafeCoerce (return .  negate :: Double -> IO Double))
    , ("Double.abs",      unsafeCoerce (return .  abs    :: Double -> IO Double))
    , ("Double.signum",   unsafeCoerce (return .  signum :: Double -> IO Double))

    , ("Double.==",       unsafeCoerce (return .: (==) :: Double -> Double -> IO Bool))
    , ("Double./=",       unsafeCoerce (return .: (/=) :: Double -> Double -> IO Bool))
    , ("Double.<",        unsafeCoerce (return .: (<)  :: Double -> Double -> IO Bool))
    , ("Double.<=",       unsafeCoerce (return .: (<=) :: Double -> Double -> IO Bool))
    , ("Double.>",        unsafeCoerce (return .: (>)  :: Double -> Double -> IO Bool))
    , ("Double.>=",       unsafeCoerce (return .: (>=) :: Double -> Double -> IO Bool))
    , ("Double.min",      unsafeCoerce (return .: min  :: Double -> Double -> IO Double))
    , ("Double.max",      unsafeCoerce (return .: max  :: Double -> Double -> IO Double))

    , ("Double.round",    unsafeCoerce (return . round   :: Double -> IO Int))
    , ("Double.ceiling",  unsafeCoerce (return . ceiling :: Double -> IO Int))
    , ("Double.floor",    unsafeCoerce (return . floor   :: Double -> IO Int))

    , ("Double.exp",      unsafeCoerce (return . exp   :: Double -> IO Double))
    , ("Double.log",      unsafeCoerce (return . log   :: Double -> IO Double))
    , ("Double.sqrt",     unsafeCoerce (return . sqrt  :: Double -> IO Double))
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

------------------
-- === Bool === --
------------------

    , ("Bool.==",       unsafeCoerce (return .: (==) :: Bool -> Bool -> IO Bool))
    , ("Bool./=",       unsafeCoerce (return .: (/=) :: Bool -> Bool -> IO Bool))
    , ("Bool.<",        unsafeCoerce (return .: (<)  :: Bool -> Bool -> IO Bool))
    , ("Bool.<=",       unsafeCoerce (return .: (<=) :: Bool -> Bool -> IO Bool))
    , ("Bool.>",        unsafeCoerce (return .: (>)  :: Bool -> Bool -> IO Bool))
    , ("Bool.>=",       unsafeCoerce (return .: (>=) :: Bool -> Bool -> IO Bool))

    , ("Bool.&&",       unsafeCoerce (return .: (&&) :: Bool -> Bool -> IO Bool))
    , ("Bool.||",       unsafeCoerce (return .: (&&) :: Bool -> Bool -> IO Bool))
    , ("Bool.not",      unsafeCoerce (return . not   :: Bool -> IO Bool))

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
    , ("String.words",    unsafeCoerce (return .  words       :: String -> IO [String]))
    , ("String.lines",    unsafeCoerce (return .  lines       :: String -> IO [String]))
    , ("String.join",     unsafeCoerce (return .: intercalate :: String -> [String] -> IO String))

------------------
-- === List === --
------------------

    , ("List.+",        unsafeCoerce (return .: (++)           :: [Any] -> [Any] -> IO [Any]))
    , ("List.length",   unsafeCoerce (return . length          :: [Any] -> IO Int))
    , ("List.reverse",  unsafeCoerce (return . reverse         :: [Any] -> IO [Any]))
    , ("List.map",      unsafeCoerce (forM                     :: [Any] -> (Any -> IO Any) -> IO [Any]))
    , ("List.fold",     unsafeCoerce ((\l i f -> foldlM f i l) :: [Any] -> Any -> (Any -> Any -> IO Any) -> IO Any))

    ]

lookupNative :: String -> Maybe Any
lookupNative = flip Map.lookup nativeCalls
