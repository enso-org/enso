{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE PolyKinds #-}

module Foreign.Storable.Utils (module Foreign.Storable.Utils, module X) where
import Foreign.Storable as X

import Prologue

import qualified Foreign.Storable.Deriving as Storable

import Foreign.Ptr      (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable, alignment, peek, peekByteOff, poke,
                         pokeByteOff, sizeOf)

data Dynamics
data Static
data Dynamic


sizeOf'    :: forall a. Storable a => Int
alignment' :: forall a. Storable a => Int
sizeOf'    = sizeOf    (undefined :: a) ; {-# INLINE sizeOf'    #-}
alignment' = alignment (undefined :: a) ; {-# INLINE alignment' #-}

castPtrTo :: forall b a. Ptr a -> Ptr b
castPtrTo = castPtr ; {-# INLINE castPtrTo #-}

intPtr :: Ptr a -> Ptr Int
intPtr = castPtrTo @Int ; {-# INLINE intPtr #-}

castPeekAndOffset :: ∀ a b m any. (MonadIO m, Storable a)
    => Ptr any -> m (a, Ptr b)
castPeekAndOffset ptr = liftIO
    $ (, ptr `plusPtr` (sizeOf' @a)) <$> peek (castPtrTo @a ptr)
{-# INLINE castPeekAndOffset #-}

castPokeAndOffset :: ∀ a b m any. (MonadIO m, Storable a)
    => Ptr any -> a -> m (Ptr b)
castPokeAndOffset ptr a = liftIO $ do
    let ptr' = castPtrTo @a ptr
    poke ptr' a
    pure $ ptr' `plusPtr` (sizeOf' @a)
{-# INLINE castPokeAndOffset #-}

peekAndOffset :: ∀ a b m. (MonadIO m, Storable a) => Ptr a -> m (a, Ptr b)
peekAndOffset = castPeekAndOffset ; {-# INLINE peekAndOffset #-}

pokeAndOffset :: ∀ a b m. (MonadIO m, Storable a) => Ptr a -> a -> m (Ptr b)
pokeAndOffset = castPokeAndOffset ; {-# INLINE pokeAndOffset #-}

Storable.derive ''Maybe

