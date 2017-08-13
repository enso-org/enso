module Data.Future where

import Prologue_old
import Control.Concurrent.MVar
import Control.Concurrent
import Data.IORef
import System.IO.Unsafe

minCapabilityNumber :: IORef Int
minCapabilityNumber = unsafePerformIO $ newIORef 0
{-# NOINLINE minCapabilityNumber #-}

availableCapabilities :: IORef [Int]
availableCapabilities = unsafePerformIO $ do
    caps <- getNumCapabilities
    newIORef $ capsList 0 caps
{-# NOINLINE availableCapabilities #-}

updateCapabilities :: IO ()
updateCapabilities = do
    caps   <- getNumCapabilities
    minCap <- readIORef minCapabilityNumber
    writeIORef availableCapabilities $ capsList minCap caps

capsList :: Int -> Int -> [Int]
capsList minCap caps | caps <= 1      = repeat 0
                     | minCap >= caps = let maxCap = caps - 1 in cycle [0..maxCap]
                     | otherwise      = let maxCap = caps - 1 in cycle [minCap..maxCap]

mkFuture :: IO (a, a -> IO ())
mkFuture = do
    v <- newEmptyMVar
    return (unsafePerformIO $ readMVar v, putMVar v)

nextCapability :: IO Int
nextCapability = atomicModifyIORef availableCapabilities $ \(a:rest) -> (rest, a)

delay :: IO a -> IO a
delay act = do
    (fut, resolve) <- mkFuture
    cap            <- nextCapability
    forkOn cap $ act >>= resolve
    return fut


resolveWith :: IO a -> (a -> IO ()) -> IO ()
resolveWith = void .: forkIO .: (>>=)
