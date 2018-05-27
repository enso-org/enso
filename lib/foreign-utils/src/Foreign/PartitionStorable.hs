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






class DynamicSubStorable a where
    sizeOf  :: a -> IO Int
    load    :: DynamicPtr -> Ptr a -> IO ()
    dump    :: DynamicPtr -> Ptr a -> IO ()

    sizeOf = \_   -> pure 0  ; {-# INLINE sizeOf #-}
    load   = \_ _ -> pure () ; {-# INLINE load   #-}
    dump   = \_ _ -> pure () ; {-# INLINE dump   #-}


instance DynamicSubStorable Bool
instance DynamicSubStorable Char
instance DynamicSubStorable Int
instance DynamicSubStorable Word16
instance DynamicSubStorable Word32
instance DynamicSubStorable Word64
instance DynamicSubStorable Word8
