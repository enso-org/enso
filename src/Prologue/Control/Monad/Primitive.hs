{-# LANGUAGE UndecidableInstances #-}

module Prologue.Control.Monad.Primitive (module Prologue.Control.Monad.Primitive, module X) where

import qualified Control.Monad.IO.Class  as IOClass

import Prelude
import Prelude                 as X (IO)
import Control.Monad.Primitive as X (PrimMonad (PrimState, primitive), RealWorld, primitive_, PrimBase, liftPrim)

import qualified Control.Monad.IO.Class  as IOClass


-- === Definitions === --

type PrimMonadST s m = (PrimMonad m, PrimState m ~ s)


-- === Utils === --

type MonadIO m = (PrimMonadST RealWorld m, IOClass.MonadIO m)

liftIO :: MonadIO m => IO a -> m a
liftIO = liftPrim ; {-# INLINE liftIO #-}

-- liftPrimIO :: MonadIO m => IO a -> m a
-- liftPrimIO = liftPrim ; {-# INLINE liftPrimIO #-}


-- -- === Default instances === --
--
-- instance {-# OVERLAPPABLE #-} (PrimMonadIO m, Monad m) => MonadIO m where
--   liftIO = liftPrimIO ; {-# INLINE liftIO #-}
