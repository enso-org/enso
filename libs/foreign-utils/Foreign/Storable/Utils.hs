module Foreign.Storable.Utils (module Foreign.Storable.Utils, module X) where

import qualified Foreign.Storable as X

import Prelude
import Foreign.Ptr      (Ptr, castPtr)
import Foreign.Storable (Storable, alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf)

sizeOf'    :: forall a. Storable a => Int
alignment' :: forall a. Storable a => Int
sizeOf'    = sizeOf    (undefined :: a) ; {-# INLINE sizeOf'    #-}
alignment' = alignment (undefined :: a) ; {-# INLINE alignment' #-}

castPtrTo :: forall b a. Ptr a -> Ptr b
castPtrTo = castPtr ; {-# INLINE castPtrTo #-}

intPtr :: Ptr a -> Ptr Int
intPtr = castPtrTo @Int ; {-# INLINE intPtr #-}
