{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE PolyKinds #-}

module Control.Monad.Exception.IO where

import Prologue

import qualified Control.Monad.Exception as Exception

import Control.Monad.Exception (MonadException)



--------------------------
-- === IO Utilities === --
--------------------------

-- === Definition === --

class MonadIO m => RethrowFromIO (t :: k) m where
    rethrowFromIO :: forall a . IO a -> m a

instance (Exception t, MonadIO m, MonadException t m)
    => RethrowFromIO (t :: Type) m where
        rethrowFromIO = rethrowFromIO_ @t
        {-# INLINE rethrowFromIO #-}

instance (MonadIO m) => RethrowFromIO '[] m where
    rethrowFromIO = liftIO
    {-# INLINE rethrowFromIO #-}

instance (Exception t, MonadIO m, MonadException t m, RethrowFromIO ts m)
    => RethrowFromIO '[t : ts] m where
        rethrowFromIO = rethrowFromIO @ts . rethrowFromIO @t
        {-# INLINE rethrowFromIO #-}


-- === API === --

rethrowFromIO_ :: forall e a m. (Exception e, MonadIO m, MonadException e m)
    => IO a -> m a
rethrowFromIO_ = \f -> liftIO (catchEitherIO @e f) >>= \case
    Left ex -> Exception.throw ex
    Right a -> pure a
{-# INLINE rethrowFromIO_ #-}

catchEitherIO :: Exception e => IO a -> IO (Either e a)
catchEitherIO = \f -> catch (Right <$> f) (pure . Left)
{-# INLINE catchEitherIO #-}

