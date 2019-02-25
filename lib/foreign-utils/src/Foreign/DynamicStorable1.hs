{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Foreign.DynamicStorable1 where

import Prologue

import qualified Foreign.Storable1      as Storable1

import Foreign.Ptr       (Ptr)
import Foreign.Storable1 (Storable1)



------------------------------
-- === DynamicStorable1 === --
------------------------------

-- === Definition === --

class DynamicStorable1 (a :: Type -> Type) where
    sizeOf :: ∀ t1. a t1 -> IO Int
    peek   :: ∀ t1. Ptr (a t1) -> IO (a t1)
    poke   :: ∀ t1. Ptr (a t1) -> a t1 -> IO ()


-- === Default instances === --

instance {-# OVERLAPPABLE #-} Storable1 t
      => DynamicStorable1 t where
    sizeOf = pure . Storable1.sizeOf ; {-# INLINE sizeOf #-}
    peek   = Storable1.peek          ; {-# INLINE peek   #-}
    poke   = Storable1.poke          ; {-# INLINE poke   #-}

