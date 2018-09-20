{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Foreign.DynamicStorable where

import Prologue

import qualified Foreign.Storable       as Storable
import qualified Foreign.Storable.Utils as Storable

import Foreign.Ptr      (Ptr)
import Foreign.Storable (Storable)



-----------------------------
-- === DynamicStorable === --
-----------------------------

-- === Definition === --

class DynamicStorable a where
    sizeOf :: a -> IO Int
    peek   :: Ptr a -> IO a
    poke   :: Ptr a -> a -> IO ()


-- === Default instances === --

-- instance {-# OVERLAPPABLE #-} Storable a
--       => DynamicStorable a where
--     sizeOf = pure . Storable.sizeOf ; {-# INLINE sizeOf #-}
--     peek   = Storable.peek          ; {-# INLINE peek   #-}
--     poke   = Storable.poke          ; {-# INLINE poke   #-}

