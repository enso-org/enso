{-# LANGUAGE CPP                  #-}
{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Foreign.Storable.Class where

#include "MachDeps.h"
#include "HsBaseConfig.h"

import Prologue

import qualified Foreign.Storable as Storable
import qualified Memory.Data.Ptr  as Memory
import qualified Type.Known       as Type

import Foreign.Ptr       (FunPtr, Ptr, plusPtr)
import Foreign.StablePtr (StablePtr)



------------------------
-- === Size types === --
------------------------

data Static
data Dynamic
data Total



-------------------------------
-- === KnownConstantSize === --
-------------------------------

-- === Class == --

class KnownConstantSize a where
    constantSize :: Int


-- === Instances === --

instance KnownConstantSize '[] where
    constantSize = 0
    {-# INLINE constantSize #-}

instance (KnownConstantSize a, KnownConstantSize as)
      => KnownConstantSize (a ': as) where
    constantSize = constantSize @a + constantSize @as
    {-# INLINE constantSize #-}


instance KnownConstantSize (Memory.Ptr t a) where
    constantSize = constantSize @(Ptr ())
    {-# INLINE constantSize #-}

-- type family ConstantSize (t :: Type) (a :: k) :: Nat


-- -- === API === --

-- type KnownConstantSize        t a = Type.KnownInt (ConstantSize t a)
-- type KnownConstantSize    a = KnownConstantSize Static  a
-- type KnownConstantDynamicSize   a = KnownConstantSize Dynamic a
-- type KnownConstantTotalSize     a = KnownConstantSize Total   a

-- constantSize :: ∀ (t :: Type) a. KnownConstantSize t a => Int
-- constantSize = Type.val' @(ConstantSize t a)
-- {-# INLINE constantSize #-}

-- constantSize  :: ∀ a. KnownConstantSize  a => Int
-- constantDynamicSize :: ∀ a. KnownConstantDynamicSize a => Int
-- constantTotalSize   :: ∀ a. KnownConstantTotalSize   a => Int
-- constantSize  = constantSize @Static  @a
-- constantDynamicSize = constantSize @Dynamic @a
-- constantTotalSize   = constantSize @Total   @a
-- {-# INLINE constantSize  #-}
-- {-# INLINE constantDynamicSize #-}
-- {-# INLINE constantTotalSize   #-}


-- -- === Instances === --

-- type instance ConstantSize t '[]       = 0
-- type instance ConstantSize t (a ': as) = ConstantSize t a + ConstantSize t as



-----------------------
-- === KnownSize === --
-----------------------

-- === Class === --

class KnownSize  (t :: Type) m a where size  ::           a       -> m Int
class KnownSize1 (t :: Type) m a where size1 :: ∀ t1    . a t1    -> m Int
class KnownSize2 (t :: Type) m a where size2 :: ∀ t1 t2 . a t1 t2 -> m Int


-- === Size types === --

type KnownStaticSize  = KnownSize Static
type KnownDynamicSize = KnownSize Dynamic
type KnownTotalSize   = KnownSize Total

staticSize  :: KnownStaticSize  m a => a -> m Int
dynamicSize :: KnownDynamicSize m a => a -> m Int
totalSize   :: KnownTotalSize   m a => a -> m Int
staticSize  = size @Static  ; {-# INLINE staticSize  #-}
dynamicSize = size @Dynamic ; {-# INLINE dynamicSize #-}
totalSize   = size @Total   ; {-# INLINE totalSize   #-}


-- === Size1 types === --

type KnownStaticSize1  = KnownSize1 Static
type KnownDynamicSize1 = KnownSize1 Dynamic
type KnownTotalSize1   = KnownSize1 Total

staticSize1  :: KnownStaticSize1  m a => a t1 -> m Int
dynamicSize1 :: KnownDynamicSize1 m a => a t1 -> m Int
totalSize1   :: KnownTotalSize1   m a => a t1 -> m Int
staticSize1  = size1 @Static  ; {-# INLINE staticSize1  #-}
dynamicSize1 = size1 @Dynamic ; {-# INLINE dynamicSize1 #-}
totalSize1   = size1 @Total   ; {-# INLINE totalSize1   #-}


-- === Size1 types === --

type KnownStaticSize2  = KnownSize2 Static
type KnownDynamicSize2 = KnownSize2 Dynamic
type KnownTotalSize2   = KnownSize2 Total

staticSize2  :: KnownStaticSize2  m a => a t1 t2 -> m Int
dynamicSize2 :: KnownDynamicSize2 m a => a t1 t2 -> m Int
totalSize2   :: KnownTotalSize2   m a => a t1 t2 -> m Int
staticSize2  = size2 @Static  ; {-# INLINE staticSize2  #-}
dynamicSize2 = size2 @Dynamic ; {-# INLINE dynamicSize2 #-}
totalSize2   = size2 @Total   ; {-# INLINE totalSize2   #-}


-- === Defaults === --

instance {-# OVERLAPPABLE #-}
    (KnownConstantSize a, Applicative m)
      => KnownSize Static m a where
    size = \_ -> pure $ constantSize @a
    {-# INLINE size #-}

instance {-# OVERLAPPABLE #-}
    (KnownStaticSize m a, KnownDynamicSize m a, Applicative m)
      => KnownSize Total m a where
    size = \a -> (+) <$> staticSize a <*> dynamicSize a
    {-# INLINE size #-}

instance {-# OVERLAPPABLE #-}
    (KnownStaticSize1 m a, KnownDynamicSize1 m a, Applicative m)
      => KnownSize1 Total m a where
    size1 = \a -> (+) <$> staticSize1 a <*> dynamicSize1 a
    {-# INLINE size1 #-}

instance {-# OVERLAPPABLE #-} KnownSize1 t m a
      => KnownSize t m (a s) where
    size = size1 @t
    {-# INLINE size #-}

instance {-# OVERLAPPABLE #-} KnownSize2 t m a
      => KnownSize1 t m (a s) where
    size1 = size2 @t
    {-# INLINE size1 #-}


----------------------
-- === Storable === --
----------------------

-- === Definition === --

type Storable t m a = (Peek t m a, Poke t m a)

class Peek (t :: Type) m a where
    peek :: Memory.UnmanagedPtr a -> m a

    default peek :: (Storable.Storable a, MonadIO m)
                 => Memory.UnmanagedPtr a -> m a
    peek = liftIO . Storable.peek . Memory.toRawPtr
    {-# INLINE peek #-}


class Poke t m a where
    poke :: Memory.UnmanagedPtr a -> a -> m ()

    default poke :: (Storable.Storable a, MonadIO m)
                 => Memory.UnmanagedPtr a -> a -> m ()
    poke = liftIO .: (Storable.poke . Memory.toRawPtr)
    {-# INLINE poke #-}


-- === API === --

type StaticPeek t m a = (Peek t m a, KnownConstantSize a)
type StaticPoke t m a = (Poke t m a, KnownConstantSize a)

peekByteOff :: ∀ t m a. Peek t m a       => Memory.UnmanagedPtr a -> Int -> m a
pokeByteOff :: ∀ t m a. Poke t m a       => Memory.UnmanagedPtr a -> Int -> a -> m ()
peekElemOff :: ∀ t m a. StaticPeek t m a => Memory.UnmanagedPtr a -> Int -> m a
pokeElemOff :: ∀ t m a. StaticPoke t m a => Memory.UnmanagedPtr a -> Int -> a -> m ()
peekElemOff = \ptr i -> peekByteOff @t ptr (i * constantSize @a)
pokeElemOff = \ptr i -> pokeByteOff @t ptr (i * constantSize @a)
peekByteOff = peek @t .: Memory.plus
pokeByteOff = poke @t .: Memory.plus
{-# INLINE peekByteOff #-}
{-# INLINE pokeByteOff #-}
{-# INLINE peekElemOff #-}
{-# INLINE pokeElemOff #-}



-- === Standard Instances === --

instance MonadIO m => Peek t m (Memory.UnmanagedPtr a) where
    peek = fmap wrap . peek . coerce
    {-# INLINE peek #-}

instance MonadIO m => Poke t m (Memory.UnmanagedPtr a) where
    poke = \ptr -> poke (coerce ptr) . unwrap
    {-# INLINE poke #-}


#define STORABLE(tp,size,aligment) \
instance MonadIO m => Peek t m (tp) ; \
instance MonadIO m => Poke t m (tp) ; \
instance KnownConstantSize (tp) where constantSize = size ; {-# INLINE constantSize #-}

STORABLE(Char,SIZEOF_INT32,ALIGNMENT_INT32)
STORABLE(Int,SIZEOF_HSINT,ALIGNMENT_HSINT)
STORABLE(Word,SIZEOF_HSWORD,ALIGNMENT_HSWORD)
STORABLE((Ptr a),SIZEOF_HSPTR,ALIGNMENT_HSPTR)
STORABLE((FunPtr a),SIZEOF_HSFUNPTR,ALIGNMENT_HSFUNPTR)
STORABLE((StablePtr a),SIZEOF_HSSTABLEPTR,ALIGNMENT_HSSTABLEPTR)
STORABLE(Float,SIZEOF_HSFLOAT,ALIGNMENT_HSFLOAT)
STORABLE(Double,SIZEOF_HSDOUBLE,ALIGNMENT_HSDOUBLE)
STORABLE(Word8,SIZEOF_WORD8,ALIGNMENT_WORD8)
STORABLE(Word16,SIZEOF_WORD16,ALIGNMENT_WORD16)
STORABLE(Word32,SIZEOF_WORD32,ALIGNMENT_WORD32)
STORABLE(Word64,SIZEOF_WORD64,ALIGNMENT_WORD64)
STORABLE(Int8,SIZEOF_INT8,ALIGNMENT_INT8)
STORABLE(Int16,SIZEOF_INT16,ALIGNMENT_INT16)
STORABLE(Int32,SIZEOF_INT32,ALIGNMENT_INT32)
STORABLE(Int64,SIZEOF_INT64,ALIGNMENT_INT64)



----------------------------
-- === Storable types === --
----------------------------

-- === Copy === --

-- | The 'Copy' storable type is used to peek / poke elements as separate
--   entities. If you peek an element, you can modify it and it will not affect
--   the original data source. If the element is a mutable object, it will be
--   copied.

data Copy


-- === View === --

-- | The 'View' storable type is used to peek / poke elements as in-place memory
--   view in the most performant way possible. If you peek a mutable element you
--   should not modify it, because it may affect the original data source.

data View

