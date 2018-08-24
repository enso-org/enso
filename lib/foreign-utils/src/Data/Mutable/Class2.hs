{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Mutable.Class2 where

import Prologue

import qualified Foreign.Storable.Class as Storable
import qualified Memory.Data.Ptr        as Memory
import qualified Type.Known             as Type

import Foreign.Ptr (Ptr)



--------------------------
-- === Construction === --
--------------------------

class Constructor m a where
    type family Args a
    constructor :: a -> Args a -> m ()

class CopyConstructor m a where
    -- | self -> ref -> action
    copyConstruct :: a -> a -> m ()


class Swizzle m a where
    swizzle :: a -> m ()

class Unswizzle m a where
    unswizzle :: a -> m ()

class UnswizzleRelTo m a where
    unswizzleRelTo :: Ptr () -> a -> m a

class Swizzle1 m a where
    swizzle1 :: a t1 -> m ()

class Unswizzle1 m a where
    unswizzle1 :: a t1 -> m ()


class SwizzleP m a where
    swizzleP :: a -> m a

class UnswizzleP m a where
    unswizzleP :: a -> m a


class SwizzleP1 m a where
    swizzleP1 :: a t1 -> m (a t1)

class UnswizzleP1 m a where
    unswizzleP1 :: a t1 -> m (a t1)

