module Data.Future where

import Prologue
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
    let maxCap = caps - 1
    newIORef $ cycle [0..maxCap]
{-# NOINLINE availableCapabilities #-}

updateCapabilities :: IO ()
updateCapabilities = do
    caps   <- getNumCapabilities
    minCap <- readIORef minCapabilityNumber
    let maxCap = caps - 1
    writeIORef availableCapabilities $ cycle [minCap..maxCap]

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
