{-# LANGUAGE UndecidableInstances #-}

module Foreign.PartitionStorable where

import Prologue

import Foreign.Ptr (Ptr)



-------------------------------
-- === PartitionStorable === --
-------------------------------

-- === Definition === --

type StaticPtr  = Ptr ()
type DynamicPtr = Ptr ()

class PartitionStorable a where
    staticSize  :: Int
    dynamicSize :: a -> IO Int
    peek        :: StaticPtr -> DynamicPtr -> IO (a, DynamicPtr)
    poke        :: StaticPtr -> DynamicPtr -> a -> IO DynamicPtr


-- === Utils === --

staticSizeOf :: ∀ a. PartitionStorable a => a -> Int
staticSizeOf = \_ -> staticSize @a ; {-# INLINE staticSizeOf #-}
