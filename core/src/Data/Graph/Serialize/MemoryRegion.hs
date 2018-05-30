module Data.Graph.Serialize.MemoryRegion where

import Prologue

import Foreign.ForeignPtr        (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr.Utils  (SomeForeignPtr)
import Foreign.Ptr.Utils         (SomePtr)


-- === Memory regions === --

data MemoryRegion = MemoryRegion
    { _staticMem  :: !SomeForeignPtr
    , _dynamicMem :: !SomeForeignPtr
    }
makeLenses ''MemoryRegion

data RawMemoryRegion = RawMemoryRegion
    { _staticMemPtr  :: !SomePtr
    , _dynamicMemPtr :: !SomePtr
    }
makeLenses ''RawMemoryRegion

unsafeMakeRaw :: MemoryRegion -> RawMemoryRegion
unsafeMakeRaw (MemoryRegion !staticMem !dynamicMem) =
    let staticPtr  = unsafeForeignPtrToPtr staticMem
        dynamicPtr = unsafeForeignPtrToPtr dynamicMem
    in RawMemoryRegion staticPtr dynamicPtr
{-# INLINE unsafeMakeRaw #-}

touch :: MonadIO m => MemoryRegion -> m ()
touch (MemoryRegion !staticMem !dynamicMem) = liftIO $! do
    touchForeignPtr staticMem
    touchForeignPtr dynamicMem
{-# INLINE touch #-}

withRaw :: MonadIO m
        => MemoryRegion -> (RawMemoryRegion -> m RawMemoryRegion)
        -> m MemoryRegion
withRaw memReg f = do
    let rawMemReg = unsafeMakeRaw memReg
    !_ <- f rawMemReg
    touch memReg
    pure $! memReg
{-# INLINE withRaw #-}
