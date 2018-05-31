{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Store.Alloc where

import Prologue

import qualified Data.Graph.Data.Component.Class as Component
import qualified Data.Graph.Data.Component.List  as ComponentList
import qualified Data.Graph.Fold.Partition       as Partition
import qualified Data.Graph.Store.External       as External
import qualified Data.TypeMap.Strict             as TypeMap
import qualified Foreign.Info.ByteSize           as ByteSize

import Data.Graph.Data.Component.Class (Component)
import Data.Graph.Data.Component.List  (ComponentList, ComponentLists)
import Data.Graph.Store.MemoryRegion   (MemoryRegion (MemoryRegion))
import Foreign.ForeignPtr.Utils        (mallocForeignPtrBytes, plusForeignPtr)



------------------
-- === Size === --
------------------

-- === Definition === --

data Size = Size
    { _staticSize   :: !Int
    , _externalSize :: !Int
    }
makeLenses ''Size


-- === API === --

totalSize :: Size -> Int
totalSize = \(Size s e) -> s + e
{-# INLINE totalSize #-}


-- === Instances === --

instance Mempty Size where
    mempty = Size 0 0
    {-# INLINE mempty #-}

instance Semigroup Size where
    (<>) = \(Size !s1 !d1) (Size !s2 !d2) ->
        let s = s1 + s2
            d = d1 + d2
        in Size s d
    {-# INLINE (<>) #-}



-----------------------
-- === Discovery === --
-----------------------

-- === Component === --

type ComponentSizeDiscovery comp m =
    ( External.SizeDiscovery1 m (Component comp)
    , ByteSize.Known (Component comp) m
    )

componentSize :: ∀ comp m. ComponentSizeDiscovery comp m
    => Component.Some comp -> m Size
componentSize = \comp -> do
    staticSize   <- ByteSize.get @(Component comp)
    externalSize <- External.size comp
    pure $! Size staticSize externalSize
{-# INLINE componentSize #-}


-- === Cluster size discovery === --

type ClusterSizeDiscovery comps m = ClusterSizeBuilder comps comps m

clusterSize :: ∀ comps m. (ClusterSizeDiscovery comps m, MonadIO m)
     => Partition.Clusters comps -> m Size
clusterSize clusters = buildClusterSize @comps @comps clusters mempty
{-# INLINE clusterSize #-}

class ClusterSizeBuilder (cs :: [Type]) comps m where
    buildClusterSize :: Partition.Clusters comps -> Size -> m Size

instance Applicative m => ClusterSizeBuilder '[] ts m where
    buildClusterSize = \_ -> pure ; {-# INLINE buildClusterSize #-}

instance
    ( TypeMap.ElemGetter (ComponentList comp) (ComponentLists comps)
    , ByteSize.Known (Component comp) m
    , ComponentSizeDiscovery comp m
    , ClusterSizeBuilder cs comps m
    , MonadIO m
    ) => ClusterSizeBuilder (comp ': cs) comps m where
    buildClusterSize clusters acc = do
        let compList    = TypeMap.getElem @(ComponentList comp) clusters
        listSize <- ComponentList.foldlM accComponentSize mempty compList
        buildClusterSize @cs @comps clusters $! acc <> listSize
    {-# INLINE buildClusterSize #-}

accComponentSize :: ∀ comp m. ComponentSizeDiscovery comp m
    => Size -> Component.Some comp -> m Size
accComponentSize = \acc -> fmap (acc <>) . componentSize
{-# INLINE accComponentSize #-}



-------------------------------
-- === Memory allocation === --
-------------------------------

type Allocator comps m =
    ( ClusterSizeDiscovery comps m
    , MonadIO m
    )

alloc :: ∀ comps m. Allocator comps m
      => Partition.Clusters comps -> m MemoryRegion
alloc clusters = do
    Size stSize dynSize <- clusterSize @comps clusters
    let totalSize = stSize + dynSize
    ptr <- liftIO $! mallocForeignPtrBytes totalSize
    pure $! MemoryRegion ptr
         $! ptr `plusForeignPtr` stSize
{-# INLINE alloc #-}
