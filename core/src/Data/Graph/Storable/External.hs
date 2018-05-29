{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Storable.External where

import Prologue

import qualified Data.Graph.Data.Component.Set    as Component
import qualified Data.Graph.Data.Component.Set    as ComponentSet
import qualified Data.Graph.Data.Component.Vector as ComponentVector
import qualified Data.Graph.Data.Layer.Class      as Layer
import qualified Data.Graph.Traversal.Fold        as Fold
import qualified Data.Graph.Traversal.Scoped      as Fold
import qualified Foreign.DynamicStorable          as DynamicStorable
import qualified Foreign.Storable.Utils           as Storable

import Data.Graph.Data.Component.Class (Component)
import Data.PtrSet.Mutable             (IsPtr, UnmanagedPtrSet)
import Data.Vector.Storable.Foreign    (Vector)
import Foreign.DynamicStorable         (DynamicStorable)
import Foreign.Ptr                     (Ptr, plusPtr)
import Foreign.Ptr.Utils               (SomePtr)
import Foreign.Storable.Utils          (Storable)



----------------------
-- === Measured === --
----------------------

-- === Definition === --

-- class Measured a where
--     byteSize :: a -> IO Int

-- class SizeBuilder a where
--     sizeBuilder :: a -> IO Int -> IO Int
--     sizeBuilder = \_ -> id ; {-# INLINE sizeBuilder #-}

-- class SizeBuilder1 a where
--     sizeBuilder1 :: ∀ t1. a t1 -> IO Int -> IO Int
--     sizeBuilder1 = \_ -> id ; {-# INLINE sizeBuilder1 #-}


-- === API === --

-- size  :: SizeBuilder  a => a    -> IO Int
-- size1 :: SizeBuilder1 a => a t1 -> IO Int
-- size  = \a -> sizeBuilder  a $! pure 0 ; {-# INLINE size  #-}
-- size1 = \a -> sizeBuilder1 a $! pure 0 ; {-# INLINE size1 #-}


-- === Instances === --

-- instance SizeBuilder Bool
-- instance SizeBuilder Char
-- instance SizeBuilder Int
-- instance SizeBuilder Word16
-- instance SizeBuilder Word32
-- instance SizeBuilder Word64
-- instance SizeBuilder Word8

-- instance SizeBuilder  (Component comp layout)
-- instance SizeBuilder1 (Component comp)


class ExternalSizeBuilder a where
    externalSizeBuilder :: a -> IO Int -> IO Int

class ExternalSizeBuilder1 a where
    externalSizeBuilder1 :: ∀ t1. a t1 -> IO Int -> IO Int

instance (Storable a, IsPtr a) => ExternalSizeBuilder (UnmanagedPtrSet a) where
    externalSizeBuilder = \a mi -> (+) <$> mi <*> DynamicStorable.sizeOf a
    {-# INLINE externalSizeBuilder #-}

instance Storable a => ExternalSizeBuilder (Vector a) where
    externalSizeBuilder = \a mi -> (+) <$> mi <*> DynamicStorable.sizeOf a
    {-# INLINE externalSizeBuilder #-}



instance ExternalSizeBuilder1 (Component.Set comp) where
    externalSizeBuilder1 = externalSizeBuilder . unwrap
    {-# INLINE externalSizeBuilder1 #-}

instance ExternalSizeBuilder1 (ComponentVector.Vector comp) where
    externalSizeBuilder1 = externalSizeBuilder . unwrap
    {-# INLINE externalSizeBuilder1 #-}



-- instance (dynamics ~ Storable.Dynamics a, SizeBuilder__ dynamics a)
--       => SizeBuilder a where
--     sizeBuilder = sizeBuilder__ @dynamics
--     {-# INLINE sizeBuilder #-}

class SB a where sb :: a -> IO Int -> IO Int
instance (dynamics ~ Storable.Dynamics a, SizeBuilder__ dynamics a)
      => SB a where sb = sizeBuilder__ @dynamics ; {-# INLINE sb #-}

class SB1 a where sb1 :: ∀ t1. a t1 -> IO Int -> IO Int
instance (dynamics ~ Storable.Dynamics a, SizeBuilder1__ dynamics a)
      => SB1 a where sb1 = sizeBuilder1__ @dynamics ; {-# INLINE sb1 #-}

size  :: ∀ a. SizeBuilder__ (Storable.Dynamics a) a => a -> IO Int
size = \a -> sizeBuilder__ @(Storable.Dynamics a) a $! pure 0 ; {-# INLINE size #-}

type S1 a = SizeBuilder1__ (Storable.Dynamics a) a
size1  :: ∀ a t1. SizeBuilder1__ (Storable.Dynamics a) a => a t1 -> IO Int
size1 = \a -> sizeBuilder1__ @(Storable.Dynamics a) a $! pure 0 ; {-# INLINE size1 #-}

class SizeBuilder__ (dynamics :: Storable.DynamicsType) a where
    sizeBuilder__ :: a -> IO Int -> IO Int

instance SizeBuilder__ 'Storable.Static a where
    sizeBuilder__ = \_ -> id ; {-# INLINE sizeBuilder__ #-}

instance ExternalSizeBuilder a => SizeBuilder__ 'Storable.Dynamic a where
    sizeBuilder__ = externalSizeBuilder
    {-# INLINE sizeBuilder__ #-}


class SizeBuilder1__ (dynamics :: Storable.DynamicsType) a where
    sizeBuilder1__ :: ∀ t1. a t1 -> IO Int -> IO Int

instance SizeBuilder1__ 'Storable.Static a where
    sizeBuilder1__ = \_ -> id ; {-# INLINE sizeBuilder1__ #-}

instance ExternalSizeBuilder1 a => SizeBuilder1__ 'Storable.Dynamic a where
    sizeBuilder1__ = externalSizeBuilder1
    {-# INLINE sizeBuilder1__ #-}


-- data DynamicsTarget (dyn :: Storable.DynamicsType)

-- instance Fold.Builder


---------------------------------
-- === ExternalStorable === --
---------------------------------

-- === Definition === --

-- | The 'load' and 'dump' functions allow loading and storing dynamic parts
--   of a structure to a given memory chunk.
class ExternalStorable a where
    loadBuilder :: Ptr a -> IO SomePtr -> IO SomePtr
    dumpBuilder :: Ptr a -> IO SomePtr -> IO SomePtr

    loadBuilder = \_ -> id ; {-# INLINE loadBuilder #-}
    dumpBuilder = \_ -> id ; {-# INLINE dumpBuilder #-}


-- === API === --

load :: (MonadIO m, ExternalStorable a) => Ptr a -> SomePtr -> m SomePtr
dump :: (MonadIO m, ExternalStorable a) => Ptr a -> SomePtr -> m SomePtr
load = \ptr -> liftIO . loadBuilder ptr . pure ; {-# INLINE load #-}
dump = \ptr -> liftIO . dumpBuilder ptr . pure ; {-# INLINE dump #-}


-- === Instances === --

instance ExternalStorable Bool
instance ExternalStorable Char
instance ExternalStorable Int
instance ExternalStorable Word16
instance ExternalStorable Word32
instance ExternalStorable Word64
instance ExternalStorable Word8
instance ExternalStorable (Component comp layout)



-----------------------------------
-- === ExternalFieldStorable === --
-----------------------------------

class ExternalFieldStorable a where
    loadFieldBuilder :: IO (a, SomePtr) -> IO (a, SomePtr)
    dumpFieldBuilder :: a -> IO SomePtr -> IO SomePtr

    loadFieldBuilder = id       ; {-# INLINE loadFieldBuilder #-}
    dumpFieldBuilder = \_ -> id ; {-# INLINE dumpFieldBuilder #-}


loadField :: ExternalFieldStorable a => a -> SomePtr -> IO (a, SomePtr)
dumpField :: ExternalFieldStorable a => a -> SomePtr -> IO SomePtr
loadField = \a dynPtr -> loadFieldBuilder $ pure (a, dynPtr) ; {-# INLINE loadField #-}
dumpField = \a dynPtr -> dumpFieldBuilder a $ pure dynPtr    ; {-# INLINE dumpField #-}

instance ExternalFieldStorable Bool
instance ExternalFieldStorable Char
instance ExternalFieldStorable Int
instance ExternalFieldStorable Word16
instance ExternalFieldStorable Word32
instance ExternalFieldStorable Word64
instance ExternalFieldStorable Word8
instance ExternalFieldStorable (Component comp layout)



-----------------------
-- === Instances === --
-----------------------

-- === UnmanagedPtrSet === --

instance (Storable a, IsPtr a) => ExternalStorable (UnmanagedPtrSet a) where
    loadBuilder = \ptr mdynPtr -> do
        dynPtr <- mdynPtr
        a      <- DynamicStorable.peek (coerce dynPtr)
        Storable.poke ptr a
        (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
    {-# INLINE loadBuilder #-}

    dumpBuilder = \ptr mdynPtr -> do
        dynPtr <- mdynPtr
        a      <- Storable.peek ptr
        DynamicStorable.poke (coerce dynPtr) a
        (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
    {-# INLINE dumpBuilder #-}

instance (Storable a, IsPtr a) => ExternalFieldStorable (UnmanagedPtrSet a) where
    loadFieldBuilder = \mdata -> do
        (!_, !dynPtr) <- mdata
        a <- DynamicStorable.peek (coerce dynPtr)
        dynPtr' <- (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
        pure (a, dynPtr')
    {-# INLINE loadFieldBuilder #-}

    dumpFieldBuilder = \a mdynPtr -> do
        dynPtr <- mdynPtr
        DynamicStorable.poke (coerce dynPtr) a
        (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
        dynPtr' <- (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
        pure dynPtr'
    {-# INLINE dumpFieldBuilder #-}

deriving instance ExternalFieldStorable (ComponentSet.Set comp layout)


-- === Vector === --

instance Storable a => ExternalStorable (Vector a) where
    loadBuilder = \ptr mdynPtr -> do
        dynPtr <- mdynPtr
        a      <- DynamicStorable.peek (coerce dynPtr)
        Storable.poke ptr a
        (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
    {-# INLINE loadBuilder #-}

    dumpBuilder = \ptr mdynPtr -> do
        dynPtr <- mdynPtr
        a      <- Storable.peek ptr
        DynamicStorable.poke (coerce dynPtr) a
        (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
    {-# INLINE dumpBuilder #-}

instance Storable a => ExternalFieldStorable (Vector a) where
    loadFieldBuilder = \mdata -> do
        (!_, !dynPtr) <- mdata
        a <- DynamicStorable.peek (coerce dynPtr)
        dynPtr' <- (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
        pure (a, dynPtr')
    {-# INLINE loadFieldBuilder #-}

    dumpFieldBuilder = \a mdynPtr -> do
        dynPtr <- mdynPtr
        DynamicStorable.poke (coerce dynPtr) a
        (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
        dynPtr' <- (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
        pure dynPtr'
    {-# INLINE dumpFieldBuilder #-}

deriving instance ExternalFieldStorable (ComponentVector.Vector comp layout)


-----------------------
-- === Discovery === --
-----------------------

data Discovery
type instance Fold.Result     Discovery = Int
type instance Fold.LayerScope Discovery = 'Fold.All

componentSize ::
    (Fold.Builder (Fold.Scoped Discovery) m (Component comp layout))
    => Component comp layout -> m Int
componentSize a = Fold.build @(Fold.Scoped Discovery) a (pure 0)
{-# INLINE componentSize #-}

instance (MonadIO m, S1 (Layer.Cons layer))
      => Fold.LayerBuilder Discovery m layer where
    layerBuild = \layer msize -> (+) <$> msize <*> liftIO (size1 layer)
    {-# INLINE layerBuild #-}
