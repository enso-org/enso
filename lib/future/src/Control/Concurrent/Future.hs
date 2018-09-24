{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Control.Concurrent.Future where

import Prologue

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar  as MVar

import Control.Concurrent.Async (Async)
import Control.Concurrent.MVar  (MVar)
import System.IO.Unsafe         (unsafePerformIO)

data FutureState a = Suspended (IO a)
                   | Computed  a
                   | Computing (Async a)

data Future a = Future { _get :: IO a
                       , _force :: IO ()
                       } deriving Functor

instance Applicative Future where
    pure a = Future (pure a) (pure ())
    Future fg ff <*> Future ag af = Future (fg <*> ag) (ff >> af)

get :: MonadIO m => Future a -> m a
get = liftIO . _get

unsafeGet :: Future a -> a
unsafeGet = unsafePerformIO . get
{-# NOINLINE unsafeGet #-}

force :: MonadIO m => Future a -> m ()
force = liftIO . _force

fromAsync :: MonadIO m => Async a -> m (Future a)
fromAsync = make . Async.wait

make :: MonadIO m => IO a -> m (Future a)
make a = liftIO $ do
    mv <- MVar.newMVar $ Suspended a
    return $ Future (getMv mv) (void $ requestMv mv)

requestMv :: MonadIO m => MVar (FutureState a) -> m (Either (Async a) a)
requestMv mv = liftIO $ do
    state <- MVar.takeMVar mv
    case state of
        Suspended a -> do
            async <- Async.async $ do
                res <- a
                MVar.takeMVar mv
                MVar.putMVar mv $ Computed res
                return res
            MVar.putMVar mv $ Computing async
            return $ Left async
        Computing async -> do
            MVar.putMVar mv state
            return $ Left async
        Computed a -> do
            MVar.putMVar mv state
            return $ Right a

getMv :: MonadIO m => MVar (FutureState a) -> m a
getMv mv = do
    res <- requestMv mv
    case res of
        Right a -> return a
        Left  a -> liftIO $ Async.wait a

