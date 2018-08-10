{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Store.Size.Discovery where

import Prologue

import qualified Data.Graph.Data.Component.Class  as Component
import qualified Data.Graph.Data.Component.List   as ComponentList
import qualified Data.Graph.Data.Component.Set    as ComponentSet
import qualified Data.Graph.Data.Component.Vector as ComponentVector
import qualified Data.Graph.Data.Graph.Class      as Graph
import qualified Data.Graph.Data.Layer.Class      as Layer
import qualified Data.Graph.Fold.Class            as Fold
import qualified Data.Graph.Fold.Filter           as Fold
import qualified Data.Graph.Fold.Partition        as Partition
import qualified Data.Graph.Fold.Scoped           as Fold
import qualified Data.Graph.Fold.Struct           as Fold
import qualified Data.Graph.Store.Size.Class      as Size
import qualified Data.Set                         as Set
import qualified Data.TypeMap.Strict              as TypeMap
import qualified Foreign.DynamicStorable          as DynamicStorable
import qualified Foreign.Storable.Class           as Storable
import qualified Foreign.Storable.Class           as Storable
import qualified Type.Show                        as Type

import Data.Graph.Data.Component.Class       (Component)
import Data.Graph.Data.Component.List        (ComponentList, ComponentLists)
import Data.Graph.Data.Component.Set         (ComponentSet)
import Data.Graph.Data.Component.Vector      (ComponentVector)
import Data.Graph.Store.Size.Class           (DynamicSize, Size (Size))
import Data.Mutable.Storable.SmallAutoVector (SmallVectorA)
import Data.PtrSet.Mutable                   (IsPtr, UnmanagedPtrSet)
import Data.Set                              (Set)
import Data.Vector.Storable.Foreign          (Vector)
import Foreign.DynamicStorable               (DynamicStorable)
import Foreign.Ptr                           (Ptr, plusPtr)
import Foreign.Ptr.Utils                     (SomePtr)
import Foreign.Storable.Utils                (Storable)



----------------------------------
-- === DynamicSizeDiscovery === --
----------------------------------

-- === Definition === --

data Discovery
type instance Fold.Result     Discovery = DynamicSize
type instance Fold.LayerScope Discovery = 'Fold.All
-- type SizeDiscoveryBuilder1 = Fold.Builder1 SizeDiscoveryCfg
-- type SizeDiscoveryCfg      = Fold.Filter Storable.Dynamics Storable.Dynamic
--                                          DynamicDiscovery


-- === API === --

class DynamicDiscovery  m a where
    discoverDynamic :: a -> m DynamicSize

class DynamicDiscovery1 m a where
    discoverDynamic1 :: ∀ t1. a t1 -> m DynamicSize

-- instance {-# OVERLAPPABLE #-} Fold.Builder SizeDiscoveryCfg m a
--       => DynamicDiscovery m a where
--     discoverDynamic = \a -> Fold.build @SizeDiscoveryCfg a $ pure mempty
--     {-# INLINE discoverDynamic #-}

-- instance {-# OVERLAPPABLE #-} SizeDiscoveryBuilder1 m a
--       => DynamicDiscovery1 m a where
--     discoverDynamic1 = foldDiscover1
--     {-# INLINE discoverDynamic1 #-}

instance {-# OVERLAPPABLE #-} DynamicDiscovery1 m (Component comp)
      => DynamicDiscovery m (Component comp layout) where
    discoverDynamic = discoverDynamic1
    {-# INLINE discoverDynamic #-}

instance {-# OVERLAPPABLE #-}
    Fold.Builder1 (Fold.Scoped Discovery) m (Component comp)
      => DynamicDiscovery1 m (Component comp) where
    discoverDynamic1 = \a -> Fold.build1 @(Fold.Scoped Discovery) a $ pure mempty
    {-# INLINE discoverDynamic1 #-}

-- foldDiscover1 :: SizeDiscoveryBuilder1 m a => a t1 -> m DynamicSize
-- foldDiscover1 = \a -> Fold.build1 @SizeDiscoveryCfg a $ pure mempty
-- {-# INLINE foldDiscover1 #-}


-- === Instances === --

instance Monad m => Fold.ComponentBuilder Discovery m comp

instance {-# OVERLAPPABLE #-}
    (Monad m, Fold.Builder1 Discovery m (Layer.Cons layer))
      => Fold.LayerBuilder Discovery m layer where
    layerBuild = Fold.build1 @Discovery
    {-# INLINE layerBuild #-}

    -- instance (MonadIO m, SizeDiscoveryBuilder1 m (Layer.Cons layer))
--       => Fold.LayerBuilder DynamicDiscovery m layer where
--     layerBuild = \a mi -> (<>) <$> mi <*> foldDiscover1 a
--     {-# INLINE layerBuild #-}

instance MonadIO m
      => Fold.Builder1 Discovery m (ComponentSet comp) where
    build1 = \a mi -> do
        size <- Storable.dynamicSize1 a
        (Size.ptrRegion %~ (+size)) <$> mi
    {-# INLINE build1 #-}

instance MonadIO m
      => Fold.Builder1 Discovery m (ComponentVector comp) where
    build1 = \a mi -> do
        size <- Storable.dynamicSize1 a
        (Size.ptrRegion %~ (+size)) <$> mi
    {-# INLINE build1 #-}

instance (MonadIO m, Storable.KnownDynamicSize m (SmallVectorA t alloc n a))
      => Fold.Builder Discovery m (SmallVectorA t alloc n a) where
    build = \a mi -> do
        size <- Storable.dynamicSize a
        (Size.dataRegion %~ (+size)) <$> mi
    {-# INLINE build #-}

instance {-# OVERLAPPABLE #-} (Monad m, Fold.Builder Discovery m a)
      => Fold.Builder Discovery m (Maybe a) where
    build = \a x -> maybe x (\t -> Fold.build @Discovery t x) a
    {-# INLINE build #-}

instance {-# OVERLAPPABLE #-}
    ( Monad m, Fold.Builder Discovery m a
    , Fold.Builder Discovery m b
    ) => Fold.Builder Discovery m (a, b) where
    build = \(a, b) x -> Fold.build @Discovery a
                       $ Fold.build @Discovery b x
    {-# INLINE build #-}


instance {-# OVERLAPPABLE #-}
    (Monad m, Fold.Builder1 (Fold.Struct Discovery) m a)
      => Fold.Builder1 Discovery m a where
    build1 = Fold.build1 @(Fold.Struct Discovery)
    {-# INLINE build1 #-}

instance Monad m => Fold.Builder1 Discovery m (Component tag)




------------------------------------
-- === Cluster size discovery === --
------------------------------------

-- === API === --

clusterSize :: ClusterSizeDiscovery comps m
            => Partition.Clusters comps -> m Size
clusterSize = \clusters -> foldClusterSize clusters mempty
{-# INLINE clusterSize #-}

class ClusterSizeDiscovery comps m where
    foldClusterSize :: Partition.Clusters comps -> Size -> m Size

instance Applicative m
     => ClusterSizeDiscovery '[] m where
    foldClusterSize = const pure
    {-# INLINE foldClusterSize #-}

instance
    ( layers ~ Graph.ComponentLayersM m comp
    , Partition.SplitHead comp comps
    , Layer.KnownByteSize layers
    , DynamicDiscovery m (Component.Some comp)
    , ClusterSizeDiscovery comps m
    , Monad m
    ) => ClusterSizeDiscovery (comp ': comps) m where
    foldClusterSize cluster acc = do
        let (  !compSet
             , !cluster') = Partition.splitHead cluster
            sizeDiscovery = \acc -> fmap (acc <>) . discoverDynamic
            compSize      = Layer.byteSize @layers
            staticSize    = compSize * Set.size compSet
        dynamicSize <- setFoldlM sizeDiscovery mempty compSet
        foldClusterSize cluster' $! acc <> Size staticSize dynamicSize
    {-# INLINE foldClusterSize #-}


setFoldlM :: Monad m
       => (a -> t -> m a) -> a -> Set t -> m a
setFoldlM = \f z0 xs ->
    let f' x k z = f z x >>= k
    in  Set.foldr f' pure xs z0
{-# INLINE setFoldlM #-}

------------------------------------
-- === Cluster size discovery === --
------------------------------------

-- === API === --

componentCount :: ClusterSizeCount comps m
               => Partition.Clusters comps -> m [Int]
componentCount = \clusters -> foldComponentCount clusters mempty
{-# INLINE componentCount #-}

class ClusterSizeCount comps m where
    foldComponentCount :: Partition.Clusters comps -> [Int] -> m [Int]

instance Applicative m
     => ClusterSizeCount '[] m where
    foldComponentCount = const pure
    {-# INLINE foldComponentCount #-}

instance
    ( Partition.SplitHead comp comps
    , ClusterSizeCount comps m
    , Monad m
    ) => ClusterSizeCount (comp ': comps) m where
    foldComponentCount cluster acc = do
        let (!compSet,
             !cluster') = Partition.splitHead cluster
            count = Set.size compSet
        foldComponentCount cluster' $! acc <> [count]
    {-# INLINE foldComponentCount #-}



-- ------------------------------
-- -- === ExternalStorable === --
-- ------------------------------

-- -- === Definition === --

-- -- | The 'load' and 'dump' functions allow loading and storing dynamic parts
-- --   of a structure to a given memory chunk.
-- class ExternalStorable a where
--     loadBuilder :: Ptr a -> IO SomePtr -> IO SomePtr
--     dumpBuilder :: Ptr a -> IO SomePtr -> IO SomePtr

--     loadBuilder = \_ -> id ; {-# INLINE loadBuilder #-}
--     dumpBuilder = \_ -> id ; {-# INLINE dumpBuilder #-}


-- -- === API === --

-- load :: (MonadIO m, ExternalStorable a) => Ptr a -> SomePtr -> m SomePtr
-- dump :: (MonadIO m, ExternalStorable a) => Ptr a -> SomePtr -> m SomePtr
-- load = \ptr -> liftIO . loadBuilder ptr . pure ; {-# INLINE load #-}
-- dump = \ptr -> liftIO . dumpBuilder ptr . pure ; {-# INLINE dump #-}


-- -- === Instances === --

-- instance ExternalStorable Bool
-- instance ExternalStorable Char
-- instance ExternalStorable Int
-- instance ExternalStorable Word16
-- instance ExternalStorable Word32
-- instance ExternalStorable Word64
-- instance ExternalStorable Word8
-- instance ExternalStorable (Component comp layout)



-- -----------------------------------
-- -- === ExternalFieldStorable === --
-- -----------------------------------

-- class ExternalFieldStorable a where
--     loadFieldBuilder :: IO (a, SomePtr) -> IO (a, SomePtr)
--     dumpFieldBuilder :: a -> IO SomePtr -> IO SomePtr

--     loadFieldBuilder = id       ; {-# INLINE loadFieldBuilder #-}
--     dumpFieldBuilder = \_ -> id ; {-# INLINE dumpFieldBuilder #-}


-- loadField :: ExternalFieldStorable a => a -> SomePtr -> IO (a, SomePtr)
-- dumpField :: ExternalFieldStorable a => a -> SomePtr -> IO SomePtr
-- loadField = \a dynPtr -> loadFieldBuilder $ pure (a, dynPtr) ; {-# INLINE loadField #-}
-- dumpField = \a dynPtr -> dumpFieldBuilder a $ pure dynPtr    ; {-# INLINE dumpField #-}

-- instance ExternalFieldStorable Bool
-- instance ExternalFieldStorable Char
-- instance ExternalFieldStorable Int
-- instance ExternalFieldStorable Word16
-- instance ExternalFieldStorable Word32
-- instance ExternalFieldStorable Word64
-- instance ExternalFieldStorable Word8
-- instance ExternalFieldStorable (Component comp layout)



-- -----------------------
-- -- === Instances === --
-- -----------------------

-- -- === UnmanagedPtrSet === --

-- instance (Storable a, IsPtr a) => ExternalStorable (UnmanagedPtrSet a) where
--     loadBuilder = \ptr mdynPtr -> do
--         dynPtr <- mdynPtr
--         a      <- DynamicStorable.peek (coerce dynPtr)
--         Storable.poke ptr a
--         (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
--     {-# INLINE loadBuilder #-}

--     dumpBuilder = \ptr mdynPtr -> do
--         dynPtr <- mdynPtr
--         a      <- Storable.peek ptr
--         DynamicStorable.poke (coerce dynPtr) a
--         (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
--     {-# INLINE dumpBuilder #-}

-- instance (Storable a, IsPtr a) => ExternalFieldStorable (UnmanagedPtrSet a) where
--     loadFieldBuilder = \mdata -> do
--         (!_, !dynPtr) <- mdata
--         a <- DynamicStorable.peek (coerce dynPtr)
--         dynPtr' <- (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
--         pure (a, dynPtr')
--     {-# INLINE loadFieldBuilder #-}

--     dumpFieldBuilder = \a mdynPtr -> do
--         dynPtr <- mdynPtr
--         DynamicStorable.poke (coerce dynPtr) a
--         (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
--         dynPtr' <- (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
--         pure dynPtr'
--     {-# INLINE dumpFieldBuilder #-}

-- -- deriving instance ExternalFieldStorable (ComponentSet comp layout)


-- -- === Vector === --

-- instance Storable a => ExternalStorable (Vector a) where
--     loadBuilder = \ptr mdynPtr -> do
--         dynPtr <- mdynPtr
--         a      <- DynamicStorable.peek (coerce dynPtr)
--         Storable.poke ptr a
--         (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
--     {-# INLINE loadBuilder #-}

--     dumpBuilder = \ptr mdynPtr -> do
--         dynPtr <- mdynPtr
--         a      <- Storable.peek ptr
--         DynamicStorable.poke (coerce dynPtr) a
--         (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
--     {-# INLINE dumpBuilder #-}

-- instance Storable a => ExternalFieldStorable (Vector a) where
--     loadFieldBuilder = \mdata -> do
--         (!_, !dynPtr) <- mdata
--         a <- DynamicStorable.peek (coerce dynPtr)
--         dynPtr' <- (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
--         pure (a, dynPtr')
--     {-# INLINE loadFieldBuilder #-}

--     dumpFieldBuilder = \a mdynPtr -> do
--         dynPtr <- mdynPtr
--         DynamicStorable.poke (coerce dynPtr) a
--         (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
--         dynPtr' <- (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
--         pure dynPtr'
--     {-# INLINE dumpFieldBuilder #-}

-- -- deriving instance ExternalFieldStorable (ComponentVector comp layout)
