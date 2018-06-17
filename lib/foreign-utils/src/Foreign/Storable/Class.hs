{-# LANGUAGE CPP                  #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Foreign.Storable.Class where

import Prologue

import qualified Foreign.Storable as Storable
import qualified Type.Known       as Type

import Foreign.Ptr (Ptr, plusPtr)



------------------
-- === Size === --
------------------

-- === Definition === --

type family Size (t :: Type) (a :: k) :: Nat


-- === API === --

class KnownStaticSize t (a :: k) where
    staticSize :: Int

class KnownDynamicSize t (a :: k) m where
    dynamicSize :: m Int

    default dynamicSize :: (Monad m, KnownStaticSize t a) => m Int
    dynamicSize = pure $! staticSize @t @a
    {-# INLINE dynamicSize #-}


-- === Default instances for list of types === --

instance KnownStaticSize t '[] where
    staticSize = 0
    {-# INLINE staticSize #-}

instance (KnownStaticSize t a, KnownStaticSize t as)
      => KnownStaticSize t (a ': as) where
    staticSize = staticSize @t @a + staticSize @t @as
    {-# INLINE staticSize #-}

instance Applicative m
      => KnownDynamicSize t '[] m where
    dynamicSize = pure 0
    {-# INLINE dynamicSize #-}

instance (KnownDynamicSize t a m, KnownDynamicSize t as m, Applicative m)
      => KnownDynamicSize t (a ': as) m where
    dynamicSize = (+) <$> dynamicSize @t @a <*> dynamicSize @t @as
    {-# INLINE dynamicSize #-}



----------------------
-- === Storable === --
----------------------

-- === Definition === --

type Storable t m a = (Peek t m a, Poke t m a)

class Peek (t :: Type) m a where
    peek :: Ptr a -> m a

    default peek :: (Storable.Storable a, MonadIO m) => Ptr a -> m a
    peek = liftIO . Storable.peek
    {-# INLINE peek #-}


class Poke t m a where
    poke :: Ptr a -> a -> m ()

    default poke :: (Storable.Storable a, MonadIO m) => Ptr a -> a -> m ()
    poke = liftIO .: Storable.poke
    {-# INLINE poke #-}


-- === API === --

type StaticPeek t m a = (Peek t m a, KnownStaticSize t a)
type StaticPoke t m a = (Poke t m a, KnownStaticSize t a)

peekByteOff :: ∀ t m a. Peek t m a       => Ptr a -> Int -> m a
pokeByteOff :: ∀ t m a. Poke t m a       => Ptr a -> Int -> a -> m ()
peekElemOff :: ∀ t m a. StaticPeek t m a => Ptr a -> Int -> m a
pokeElemOff :: ∀ t m a. StaticPoke t m a => Ptr a -> Int -> a -> m ()
peekElemOff = \ptr i -> peekByteOff @t ptr (i * staticSize @t @a)
pokeElemOff = \ptr i -> pokeByteOff @t ptr (i * staticSize @t @a)
peekByteOff = peek @t .: plusPtr
pokeByteOff = poke @t .: plusPtr
{-# INLINE peekByteOff #-}
{-# INLINE pokeByteOff #-}
{-# INLINE peekElemOff #-}
{-# INLINE pokeElemOff #-}



-- === Standard Instances === --

#define STORABLE(tp) \
instance MonadIO m => Peek t m (tp)                        ; \
instance MonadIO m => Poke t m (tp)                        ; \
instance {-# OVERLAPPABLE #-} KnownStaticSize t (tp) where { \
    staticSize = Storable.sizeOf (undefined :: tp)         ; \
    {-# INLINE staticSize #-}                              }

STORABLE(Int)
STORABLE(Ptr a)



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
