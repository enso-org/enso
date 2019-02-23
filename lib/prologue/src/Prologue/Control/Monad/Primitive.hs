{-# LANGUAGE UndecidableInstances #-}

module Prologue.Control.Monad.Primitive (module X) where

-- import qualified Control.Monad.IO.Class  as IOClass

-- import Prelude
import Prelude                 as X (IO)
import Control.Monad.IO.Class  as X (MonadIO, liftIO)
import Control.Monad.Primitive as X (PrimMonad (PrimState, primitive), RealWorld, primitive_, PrimBase, liftPrim)

-- import qualified Control.Monad.IO.Class  as IOClass


-- -- === Definitions === --
--
-- type PrimMonadST s m = (PrimMonad m, PrimState m ~ s)
-- type MonadIO       m = PrimMonadST RealWorld m
--
--
-- -- === Utils === --
--
-- liftIO :: MonadIO m => IO a -> m a
-- liftIO = liftPrim ; {-# INLINE liftIO #-}
--
--
-- -- === Intances === --
--
-- instance {-# OVERLAPPABLE #-} (MonadIO m, Monad m) => IOClass.MonadIO m where
--     liftIO = liftIO ; {-# INLINE liftIO #-}
