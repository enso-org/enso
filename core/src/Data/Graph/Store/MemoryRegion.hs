module Data.Graph.Store.MemoryRegion where

import Prologue

import Foreign.ForeignPtr        (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr.Utils  (SomeForeignPtr)
import Foreign.Ptr.Utils         (SomePtr)


--------------------------
-- === MemoryRegion === --
--------------------------

-- === Definition === --

data MemoryRegion = MemoryRegion
    { _staticMem      :: SomeForeignPtr
    , _dynamicMem     :: SomeForeignPtr
    , _dynamicPtrsMem :: SomeForeignPtr
    } deriving (Show)
makeLenses ''MemoryRegion

data Raw = Raw
    { _staticMemPtr      :: SomePtr
    , _dynamicMemPtr     :: SomePtr
    , _dynamicPtrsMemPtr :: SomePtr
    } deriving (Show)
makeLenses ''Raw

data Dynamic = Dynamic
    { _noPointersMem :: SomePtr
    , _pointersMem   :: SomePtr
    } deriving (Show)
makeLenses ''Dynamic

type DynamicMemVariant = Lens' Dynamic SomePtr

unsafeMakeRaw :: MemoryRegion -> Raw
unsafeMakeRaw = \(MemoryRegion staticMem dynamicMem ptrsMem) ->
    let staticPtr  = unsafeForeignPtrToPtr staticMem
        dynamicPtr = unsafeForeignPtrToPtr dynamicMem
        ptrsPtr    = unsafeForeignPtrToPtr ptrsMem
    in Raw staticPtr dynamicPtr ptrsPtr
{-# INLINE unsafeMakeRaw #-}

touch :: MonadIO m => MemoryRegion -> m ()
touch = \(MemoryRegion staticMem dynamicMem ptrsMem) -> liftIO $! do
    touchForeignPtr staticMem
    touchForeignPtr dynamicMem
    touchForeignPtr ptrsMem
{-# INLINE touch #-}

withRaw :: MonadIO m
        => MemoryRegion -> (Raw -> m Raw)
        -> m MemoryRegion
withRaw = \memReg f -> do
    let rawMemReg = unsafeMakeRaw memReg
    _ <- f rawMemReg
    touch memReg
    pure $! memReg
{-# INLINE withRaw #-}

viewDynamic :: Raw -> Dynamic
viewDynamic = \(Raw _ dm dpm) -> Dynamic dm dpm
{-# INLINE viewDynamic #-}

constructRaw :: Dynamic -> SomePtr -> Raw
constructRaw = \(Dynamic dm dpm) ptr -> Raw ptr dm dpm
{-# INLINE constructRaw #-}
