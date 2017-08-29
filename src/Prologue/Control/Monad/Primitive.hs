{-# LANGUAGE UndecidableInstances #-}

module Prologue.Control.Monad.Primitive (module Prologue.Control.Monad.Primitive, module X) where

import qualified Control.Monad.IO.Class  as IOClass

import Prelude
import Prelude                 as X (IO)
import Control.Monad.IO.Class  as X (MonadIO)
import Control.Monad.Primitive as X (PrimMonad (PrimState, primitive), RealWorld, primitive_, PrimBase, liftPrim)


-- === Definitions === --

type PrimMonadST s m = (PrimMonad m, PrimState m ~ s)
type PrimMonadIO   m = (PrimMonadST RealWorld m, MonadIO m)


-- === Utils === --

{-# DEPRECATED liftIO "Use either `liftPrimIO` or `liftPrim` instead" #-}
liftIO :: MonadIO m => IO a -> m a
liftIO = IOClass.liftIO ; {-# INLINE liftIO #-}

liftPrimIO :: PrimMonadIO m => IO a -> m a
liftPrimIO = liftPrim ; {-# INLINE liftPrimIO #-}


-- === Default instances === --

instance {-# OVERLAPPABLE #-} (PrimMonadIO m, Monad m) => MonadIO m where
  liftIO = liftPrimIO ; {-# INLINE liftIO #-}
