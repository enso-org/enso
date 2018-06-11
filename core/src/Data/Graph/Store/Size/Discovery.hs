{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Store.Size.Discovery where

import Prologue

import qualified Data.Graph.Data.Component.Set    as Component
import qualified Data.Graph.Data.Component.Set    as ComponentSet
import qualified Data.Graph.Data.Component.Vector as ComponentVector
import qualified Data.Graph.Data.Layer.Class      as Layer
import qualified Data.Graph.Fold.Class            as Fold
import qualified Data.Graph.Fold.Filter           as Fold
import qualified Data.Graph.Fold.Scoped           as Fold
import qualified Data.Graph.Fold.Struct           as Fold
import qualified Data.Graph.Store.Size.Class      as Size
import qualified Foreign.DynamicStorable          as DynamicStorable
import qualified Foreign.Storable.Utils           as Storable
import qualified Type.Show                        as Type

import Data.Graph.Data.Component.Class  (Component)
import Data.Graph.Data.Component.Set    (ComponentSet)
import Data.Graph.Data.Component.Vector (ComponentVector)
import Data.Graph.Store.Size.Class      (DynamicSize)
import Data.PtrSet.Mutable              (IsPtr, UnmanagedPtrSet)
import Data.Vector.Storable.Foreign     (Vector)
import Foreign.DynamicStorable          (DynamicStorable)
import Foreign.Ptr                      (Ptr, plusPtr)
import Foreign.Ptr.Utils                (SomePtr)
import Foreign.Storable.Utils           (Storable)



---------------------------
-- === SizeDiscovery === --
---------------------------

-- === Definition === --

data Discovery
type instance Fold.Result     Discovery = DynamicSize
type instance Fold.LayerScope Discovery = 'Fold.All
type SizeDiscoveryBuilder1 = Fold.Builder1 SizeDiscoveryCfg
type SizeDiscoveryCfg      = Fold.Filter Storable.Dynamics Storable.Dynamic
                                         Discovery


-- === API === --

class SizeDiscovery  m a where size  ::       a    -> m DynamicSize
class SizeDiscovery1 m a where size1 :: ∀ t1. a t1 -> m DynamicSize

instance {-# OVERLAPPABLE #-} Fold.Builder SizeDiscoveryCfg m a
      => SizeDiscovery m a where
    size = \a -> Fold.build @SizeDiscoveryCfg a $ pure mempty
    {-# INLINE size #-}

instance {-# OVERLAPPABLE #-} SizeDiscoveryBuilder1 m a
      => SizeDiscovery1 m a where
    size1 = foldSize1
    {-# INLINE size1 #-}

instance {-# OVERLAPPABLE #-} SizeDiscovery1 m (Component comp)
      => SizeDiscovery m (Component comp layout) where
    size = size1
    {-# INLINE size #-}

instance {-# OVERLAPPABLE #-}
    Fold.Builder1 (Fold.Scoped Discovery) m (Component comp)
      => SizeDiscovery1 m (Component comp) where
    size1 = \a -> Fold.build1 @(Fold.Scoped Discovery) a $ pure mempty
    {-# INLINE size1 #-}

foldSize1 :: SizeDiscoveryBuilder1 m a => a t1 -> m DynamicSize
foldSize1 = \a -> Fold.build1 @SizeDiscoveryCfg a $ pure mempty
{-# INLINE foldSize1 #-}


-- === Instances === --

instance Monad m => Fold.ComponentBuilder Discovery m comp

instance (MonadIO m, SizeDiscoveryBuilder1 m (Layer.Cons layer))
      => Fold.LayerBuilder Discovery m layer where
    layerBuild = \a mi -> (<>) <$> mi <*> foldSize1 a
    {-# INLINE layerBuild #-}

instance MonadIO m
      => Fold.Builder1 Discovery m (ComponentSet comp) where
    build1 = \a mi -> do
        size <- liftIO (DynamicStorable.sizeOf a)
        (Size.ptrRegion %~ (+size)) <$> mi
    {-# INLINE build1 #-}

instance MonadIO m
      => Fold.Builder1 Discovery m (ComponentVector comp) where
    build1 = \a mi -> do
        size <- liftIO (DynamicStorable.sizeOf a)
        (Size.ptrRegion %~ (+size)) <$> mi
    {-# INLINE build1 #-}

instance (MonadIO m, Storable a, IsPtr a)
      => Fold.Builder Discovery m (UnmanagedPtrSet a) where
    build = \a mi -> do
        size <- liftIO (DynamicStorable.sizeOf a)
        (Size.dataRegion %~ (+size)) <$> mi
    {-# INLINE build #-}

instance (MonadIO m, Storable a)
      => Fold.Builder Discovery m (Vector a) where
    build = \a mi -> do
        size <- liftIO (DynamicStorable.sizeOf a)
        (Size.dataRegion %~ (+size)) <$> mi
    {-# INLINE build #-}

instance {-# OVERLAPPABLE #-}
    (Monad m, Fold.Builder1 (Fold.Struct SizeDiscoveryCfg) m a)
      => Fold.Builder1 Discovery m a where
    build1 = Fold.build1 @(Fold.Struct SizeDiscoveryCfg)
    {-# INLINE build1 #-}



------------------------------
-- === ExternalStorable === --
------------------------------

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

deriving instance ExternalFieldStorable (ComponentSet comp layout)


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

deriving instance ExternalFieldStorable (ComponentVector comp layout)
