module Data.Future where

import Prologue
import Control.Concurrent.MVar
import Control.Concurrent
import System.IO.Unsafe

mkFuture :: IO (a, a -> IO ())
mkFuture = do
    v <- newEmptyMVar
    return (unsafePerformIO $ readMVar v, putMVar v)

delay :: IO a -> IO a
delay act = do
    (fut, resolve) <- mkFuture
    forkIO $ act >>= resolve
    return fut


resolveWith :: IO a -> (a -> IO ()) -> IO ()
resolveWith = void .: forkIO .: (>>=)
