module Prologue.Control.Monad.Primitive (module Prologue.Control.Monad.Primitive, module X) where

import qualified Control.Monad.IO.Class  as IOClass

import Prelude                 as X (IO)
import Control.Monad.IO.Class  as X (MonadIO)
import Control.Monad.Primitive as X (PrimMonad (PrimState, primitive), RealWorld, primitive_, PrimBase, liftPrim)


type PrimMonadST s m = (PrimMonad m, PrimState m ~ s)
type PrimMonadIO   m = PrimMonadST RealWorld m

{-# DEPRECATED liftIO "Use `liftPrim` instead" #-}
liftIO :: MonadIO m => IO a -> m a
liftIO = IOClass.liftIO ; {-# INLINE liftIO #-}
