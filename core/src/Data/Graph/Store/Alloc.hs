{-# LANGUAGE Strict #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Store.Alloc where

-- import Prologue

-- import qualified Data.Graph.Data.Component.Class as Component
-- import qualified Data.Graph.Data.Component.List  as ComponentList
-- import qualified Data.Graph.Fold.Partition       as Partition
-- import qualified Data.Graph.Store.Size.Discovery as Size
-- import qualified Data.TypeMap.Strict             as TypeMap
-- import qualified Foreign.Storable.Class          as Storable
-- import qualified Data.Graph.Store.External       as External
-- import qualified Data.Graph.Store.Size           as Size
-- import qualified Foreign.Info.ByteSize           as ByteSize

-- import Data.Graph.Data.Component.Class (Component)
-- import Data.Graph.Data.Component.List (ComponentList, ComponentLists)
-- import Data.Graph.Store.Size.Class    (DynamicSize, Size (Size))
-- import Data.Graph.Store.MemoryRegion   (MemoryRegion (MemoryRegion))
-- import Data.Graph.Store.Size           (Size)
-- import Foreign.ForeignPtr.Utils        (mallocForeignPtrBytes, plusForeignPtr)


------------------
-- === Size === --
------------------

-- === Definition === --

-- data Size = Size
--     { _staticSize   :: Int
--     , _externalSize :: External.Size
--     } deriving Show
-- makeLenses ''Size


-- -- === Instances === --

-- instance Mempty Size where
--     mempty = Size 0 mempty
--     {-# INLINE mempty #-}

-- instance Semigroup Size where
--     (<>) = \(Size s1 d1) (Size s2 d2) ->
--         let s = s1 + s2
--             d = d1 <> d2
--         in Size s d
--     {-# INLINE (<>) #-}


-- === Helpers === --

-- totalSize :: Size -> Int
-- totalSize (Size static external) = static + (External.totalSize external)
-- {-# INLINE totalSize #-}

-- allocRegionForSize :: MonadIO m => Size -> m MemoryRegion
-- allocRegionForSize = \size -> do
--     ptr <- liftIO $ mallocForeignPtrBytes $! Size.total size
--     let dynSize    = size ^. Size.dynamic . Size.dataRegion
--         dynMemPtr  = plusForeignPtr ptr $! size ^. staticSize
--         ptrsMemPtr = plusForeignPtr dynMemPtr dynSize
--     pure $! MemoryRegion ptr dynMemPtr ptrsMemPtr
-- {-# INLINE allocRegionForSize #-}


-- -----------------------
-- -- === Discovery === --
-- -----------------------

-- -- === Component === --

-- type ComponentSizeDiscovery comp m =
--     ( External.SizeDiscovery1 m (Component comp)
--     , ByteSize.Known (Component comp) m
--     )

-- componentSize :: ∀ comp m. ComponentSizeDiscovery comp m
--     => Component.Some comp -> m Size
-- componentSize = \comp -> do
--     staticSize   <- ByteSize.get @(Component comp)
--     externalSize <- External.size comp
--     pure $! Size staticSize externalSize
-- {-# INLINE componentSize #-}


-- === Cluster size discovery === --


--------------------------
-- === Cluster size === --
--------------------------

-- === API === --

-- class ClusterSizeDiscovery comps m where
--     clusterSize :: Partition.Clusters comps -> Size -> m Size

-- instance Applicative m
--      => ClusterSizeDiscovery '[] m where
--     clusterSize = const pure
--     {-# INLINE clusterSize #-}

-- instance
--     ( TypeMap.SplitHead (ComponentList comp) (ComponentLists comps)
--     , Storable.KnownConstantStaticSize comp
--     , Size.DynamicDiscovery m (Component.Some comp)
--     , ClusterSizeDiscovery comps m
--     , Monad m
--     ) => ClusterSizeDiscovery (comp ': comps) m where
--     clusterSize cluster acc = do
--         let (  !compList
--              , !cluster') = Partition.splitHead cluster
--             sizeDiscovery = \acc -> fmap (acc <>) . Size.discoverDynamic
--             compSize      = Storable.constantStaticSize @comp
--             staticSize    = compSize * ComponentList.length compList
--         dynamicSize <- ComponentList.foldlM sizeDiscovery mempty compList
--         clusterSize cluster' $! acc <> Size staticSize dynamicSize
--     {-# INLINE clusterSize #-}



-- -------------------------------
-- -- === Memory allocation === --
-- -------------------------------

-- type Allocator comps m =
--     ( ClusterSizeDiscovery comps m
--     , MonadIO m
--     )

-- alloc :: ∀ comps m. Allocator comps m
--       => Partition.Clusters comps -> m MemoryRegion
-- alloc = allocRegionForSize <=< clusterSize @comps
-- {-# INLINE alloc #-}
