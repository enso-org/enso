{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Store.Size.Class where

import Prologue

import qualified Data.Graph.Data.Component.Set    as Component
import qualified Data.Graph.Data.Component.Set    as ComponentSet
import qualified Data.Graph.Data.Component.Vector as ComponentVector
import qualified Data.Graph.Data.Layer.Class      as Layer
import qualified Data.Graph.Fold.Class            as Fold
import qualified Data.Graph.Fold.Filter           as Fold
import qualified Data.Graph.Fold.Partition        as Partition
import qualified Data.Graph.Fold.Scoped           as Fold
import qualified Data.Graph.Fold.Struct           as Fold
import qualified Foreign.Storable.Deriving        as Storable
import qualified Foreign.Storable.Utils           as Storable

import Data.Graph.Data.Component.Class (Component)
import Data.Graph.Data.Component.Set   (ComponentSet)
import Data.PtrSet.Mutable             (IsPtr, UnmanagedPtrSet)
import Data.Vector.Storable.Foreign    (Vector)
import Foreign.Ptr                     (Ptr, plusPtr)
import Foreign.Ptr.Utils               (SomePtr)
import Foreign.Storable.Utils          (Storable)



------------------
-- === Size === --
------------------

-- === Definition === --

data DynamicSize = DynamicSize
    { _dataRegion :: Int
    , _ptrRegion  :: Int
    } deriving (Show)
makeLenses ''DynamicSize
Storable.derive ''DynamicSize

data Size = Size
    { _static  :: Int
    , _dynamic :: DynamicSize
    } deriving (Show)
makeLenses ''Size
Storable.derive ''Size


-- === API === --

total :: Size -> Int
total = \(Size s (DynamicSize a b)) -> s + a + b
{-# INLINE total #-}


-- === Instances === --

instance Mempty Size where
    mempty = Size 0 mempty
    {-# INLINE mempty #-}

instance Mempty DynamicSize where
    mempty = DynamicSize 0 0
    {-# INLINE mempty #-}

instance Semigroup Size where
    (<>) = \(Size s d) (Size s' d') -> Size (s + s') (d <> d')
    {-# INLINE (<>) #-}

instance Semigroup DynamicSize where
    (<>) = \(DynamicSize a b) (DynamicSize a' b')
        -> DynamicSize (a + a') (b + b')
    {-# INLINE (<>) #-}





-- -- === Cluster size discovery === --

-- type ClusterSizeBuilder comps m = ClusterSizeBuilder__ comps comps m

-- clusterSize :: ∀ comps m. (ClusterSizeBuilder comps m, MonadIO m)
--      => Partition.Clusters comps -> m Size
-- clusterSize clusters = buildClusterSize @comps @comps clusters mempty
-- {-# INLINE clusterSize #-}

-- class ClusterSizeBuilder__ (cs :: [Type]) comps m where
--     buildClusterSize :: Partition.Clusters comps -> Size -> m Size

-- instance Applicative m => ClusterSizeBuilder__ '[] ts m where
--     buildClusterSize = \_ -> pure ; {-# INLINE buildClusterSize #-}

-- instance
--     ( TypeMap.ElemGetter (ComponentList comp) (ComponentLists comps)
--     , ByteSize.Known (Component comp) m
--     , ComponentSizeDiscovery comp m
--     , ClusterSizeBuilder__ cs comps m
--     , MonadIO m
--     ) => ClusterSizeBuilder__ (comp ': cs) comps m where
--     buildClusterSize clusters size = do
--         let compList = TypeMap.getElem @(ComponentList comp) clusters
--         listSize <- ComponentList.foldlM accComponentSize mempty compList
--         buildClusterSize @cs @comps clusters $! size <> listSize
--     {-# INLINE buildClusterSize #-}

-- accComponentSize :: ∀ comp m. ComponentSizeDiscovery comp m
--     => Size -> Component.Some comp -> m Size
-- accComponentSize = \acc -> fmap (acc <>) . componentSize
-- {-# INLINE accComponentSize #-}


-- componentSize :: ∀ comp m. ComponentSizeDiscovery comp m
--     => Component.Some comp -> m Size
-- componentSize = \comp -> do
--     staticSize   <- ByteSize.get @(Component comp)
--     externalSize <- External.size comp
--     pure $! Size staticSize externalSize
-- {-# INLINE componentSize #-}
