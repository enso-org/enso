module Luna.Compilation.Pass.Interpreter.NativeCalls where

import           Prelude.Luna

import qualified Data.Map as Map
import           Data.Map (Map)
import           Unsafe.Coerce   (unsafeCoerce)
import           GHC.Prim        (Any)

nativeCalls :: Map String Any
nativeCalls = Map.fromList $
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
    ]

lookupNative :: String -> Maybe Any
lookupNative = flip Map.lookup nativeCalls
