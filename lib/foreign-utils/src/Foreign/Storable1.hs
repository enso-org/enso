{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Foreign.Storable1 where

import Data.Kind
import Prelude

import Foreign.Ptr (Ptr, castPtr, plusPtr)


class Storable1 (t :: Type -> Type) where
    sizeOf      :: ∀ a. t a -> Int
    alignment   :: ∀ a. t a -> Int
    peekElemOff :: ∀ a.   Ptr (t a) -> Int        -> IO (t a)
    pokeElemOff :: ∀ a.   Ptr (t a) -> Int -> t a -> IO ()
    peekByteOff :: ∀ a b. Ptr b     -> Int        -> IO (t a)
    pokeByteOff :: ∀ a b. Ptr b     -> Int -> t a -> IO ()
    peek        :: ∀ a.   Ptr (t a)               -> IO (t a)
    poke        :: ∀ a.   Ptr (t a)        -> t a -> IO ()

    peekElemOff = peekElemOff_ undefined where
        peekElemOff_ :: ∀ a. t a -> Ptr (t a) -> Int -> IO (t a)
        peekElemOff_ undef ptr off = peekByteOff ptr (off * sizeOf undef)
    pokeElemOff ptr off val = pokeByteOff ptr (off * sizeOf val) val
    {-# INLINE peekElemOff #-}
    {-# INLINE pokeElemOff #-}

    peekByteOff ptr off = peek (ptr `plusPtr` off) ; {-# INLINE peekByteOff #-}
    pokeByteOff ptr off = poke (ptr `plusPtr` off) ; {-# INLINE pokeByteOff #-}

    peek ptr = peekElemOff ptr 0 ; {-# INLINE peek #-}
    poke ptr = pokeElemOff ptr 0 ; {-# INLINE poke #-}


sizeOf' :: ∀ t a. Storable1 t => Int
sizeOf' = sizeOf (undefined :: t a) ; {-# INLINE sizeOf' #-}

alignment' :: ∀ t a. Storable1 t => Int
alignment' = alignment (undefined :: t a) ; {-# INLINE alignment' #-}

