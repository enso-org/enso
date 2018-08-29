{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Mutable.Plain where

import Prologue

import qualified Foreign.Storable.Class as Storable
import qualified Memory.Data.Ptr        as Memory
import qualified Type.Known             as Type


-----------------------------
-- === CopyInitializer === --
-----------------------------

-- | Initialize components after memcpy

class CopyInitializer m a where
    copyInitialize :: a -> m ()

class CopyInitializer1 m a where
    copyInitialize1 :: ∀ t1. a t1 -> m ()

