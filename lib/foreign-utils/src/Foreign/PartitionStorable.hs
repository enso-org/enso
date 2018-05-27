{-# LANGUAGE UndecidableInstances #-}

module Foreign.PartitionStorable where

import Prologue

import Foreign.Ptr (Ptr)



-- -------------------------------
-- -- === PartitionStorable === --
-- -------------------------------

-- -- === Definition === --

-- type StaticPtr  = Ptr ()
type DynamicPtr = Ptr ()

-- class PartitionStorable a where
--     staticSize  :: Int
--     dynamicSize :: a -> IO Int
--     peek        :: StaticPtr -> DynamicPtr -> IO (a, DynamicPtr)
--     poke        :: StaticPtr -> DynamicPtr -> a -> IO DynamicPtr


-- -- === Utils === --

-- staticSizeOf :: ∀ a. PartitionStorable a => a -> Int
-- staticSizeOf = \_ -> staticSize @a ; {-# INLINE staticSizeOf #-}






class ExternalStorable a where
    loadBuilder :: Ptr a -> IO DynamicPtr -> IO DynamicPtr
    dumpBuilder :: Ptr a -> IO DynamicPtr -> IO DynamicPtr

    loadBuilder = \_ -> id ; {-# INLINE loadBuilder #-}
    dumpBuilder = \_ -> id ; {-# INLINE dumpBuilder #-}

load :: ExternalStorable a => Ptr a -> DynamicPtr -> IO DynamicPtr
dump :: ExternalStorable a => Ptr a -> DynamicPtr -> IO DynamicPtr
load = \ptr -> loadBuilder ptr . pure ; {-# INLINE load #-}
dump = \ptr -> dumpBuilder ptr . pure ; {-# INLINE dump #-}


instance ExternalStorable Bool
instance ExternalStorable Char
instance ExternalStorable Int
instance ExternalStorable Word16
instance ExternalStorable Word32
instance ExternalStorable Word64
instance ExternalStorable Word8
