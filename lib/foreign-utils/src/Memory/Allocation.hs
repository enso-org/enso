{-# LANGUAGE UndecidableInstances #-}

module Memory.Allocation where

import Prologue

import qualified Foreign.Storable.Class as Storable
import qualified Memory.Data.Ptr        as Memory
import qualified Memory.Management      as Memory
import qualified Type.Data.Parameter    as Parameter



-------------------------------
-- === Memory Allocation === --
-------------------------------

-- === Definition === --

data Allocator = Allocator Type

class Allocation (alloc :: Allocator) (t :: Memory.ManagementType) a m where
    allocate :: Int -> m (Memory.Ptr t a)


-- === Aliases === --

type ManagedAllocation   alloc = Allocation alloc 'Memory.Managed
type UnmanagedAllocation alloc = Allocation alloc 'Memory.Unmanaged


-- === StdAllocator === --

data Std
type StdAllocator = 'Allocator Std
instance (MonadIO m, Storable.KnownConstantSize a, Memory.PtrType t)
      => Allocation StdAllocator t a m where
    allocate = Memory.mallocBytes . (Storable.constantSize @a *)
    {-# INLINE allocate #-}


-- === Allocator manipulation === --

type GetAllocator   t = Parameter.GetByKind Allocator   t
type SetAllocator v t = Parameter.SetByKind Allocator v t
type instance Parameter.DefaultByKind Allocator = StdAllocator

setAllocator :: ∀ alloc a. a -> SetAllocator alloc a
setAllocator = Parameter.setByKind @Allocator @alloc
{-# INLINE setAllocator #-}

