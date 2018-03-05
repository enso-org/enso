module Foreign.Storable1 where

import Prelude
import Data.Kind

import Foreign.Ptr (Ptr, castPtr, plusPtr)


class Storable1 (t :: Type -> Type) where
    sizeOf      :: Int
    alignment   :: Int
    peekElemOff :: ∀ a.   Ptr (t a) -> Int        -> IO (t a)
    pokeElemOff :: ∀ a.   Ptr (t a) -> Int -> t a -> IO ()
    peekByteOff :: ∀ a b. Ptr b     -> Int        -> IO (t a)
    pokeByteOff :: ∀ a b. Ptr b     -> Int -> t a -> IO ()
    peek        :: ∀ a.   Ptr (t a)               -> IO (t a)
    poke        :: ∀ a.   Ptr (t a)        -> t a -> IO ()

    peekElemOff ptr off     = peekByteOff ptr (off * sizeOf @t)     ; {-# INLINE peekElemOff #-}
    pokeElemOff ptr off val = pokeByteOff ptr (off * sizeOf @t) val ; {-# INLINE pokeElemOff #-}

    peekByteOff ptr off = peek (ptr `plusPtr` off) ; {-# INLINE peekByteOff #-}
    pokeByteOff ptr off = poke (ptr `plusPtr` off) ; {-# INLINE pokeByteOff #-}

    peek ptr = peekElemOff ptr 0 ; {-# INLINE peek #-}
    poke ptr = pokeElemOff ptr 0 ; {-# INLINE poke #-}
