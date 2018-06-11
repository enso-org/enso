{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Store.Internal where

import Prologue

-- import qualified Control.Monad.State.Layered    as State
-- import qualified Data.Graph.Data.Component.List as Component
-- import qualified Data.Graph.Fold.Partition      as Partition
-- import qualified Data.Graph.Store.Alloc         as Alloc
-- import qualified Data.Graph.Store.Component     as Component
-- import qualified Data.Graph.Store.MemoryRegion  as MemoryRegion
-- import qualified Data.TypeMap.Strict            as TypeMap

-- import Data.Graph.Data.Component.List (ComponentList, ComponentLists)
-- import Data.Graph.Store.Component     (ExternalStorableComponent,
--                                        ExternalStorableComponents)
-- import Data.Graph.Store.MemoryRegion  (MemoryRegion)
-- import Data.Map                       (Map)
-- import Foreign.Ptr.Utils              (SomePtr)



-- ---------------------------------------
-- -- === Serialization of Clusters === --
-- ---------------------------------------

-- -- === Iterate over Cluster and serialize each list === --

-- class ClusterSerializerBuilder (cs :: [Type]) comps m where
--     buildClusterSerializer :: Partition.Clusters comps
--                            -> MemoryRegion.Raw -> m MemoryRegion.Raw

-- instance Applicative m => ClusterSerializerBuilder '[] ts m where
--     buildClusterSerializer = \_ -> pure
--     {-# INLINE buildClusterSerializer #-}

-- instance
--     ( TypeMap.ElemGetter (ComponentList comp) (ComponentLists comps)
--     , ExternalStorableComponent comp m
--     , ClusterSerializerBuilder cs comps m
--     , State.Monad Component.PointerMap m
--     , MonadIO m
--     ) => ClusterSerializerBuilder (comp ': cs) comps m where
--     buildClusterSerializer clusters acc = do
--         let compList = TypeMap.getElem @(ComponentList comp) clusters
--         acc' <- Component.dumpComponentList compList acc
--         buildClusterSerializer @cs @comps clusters acc'
--     {-# INLINE buildClusterSerializer #-}


-- -- === API === --

-- data SerializeInfo = SerializeInfo
--     { _memoryRegion :: !MemoryRegion
--     , _ptrMapping   :: !(Map SomePtr SomePtr)
--     }
-- makeLenses ''SerializeInfo

-- type ClusterSerializer comps m =
--     ( Alloc.Allocator comps m
--     , ExternalStorableComponents comps m
--     , ClusterSerializerBuilder comps comps (State.StateT Component.PointerMap m)
--     )

-- serializeClusters :: ∀ comps m. ClusterSerializer comps m
--                   => Partition.Clusters comps -> MemoryRegion -> m SerializeInfo
-- serializeClusters clusters memReg = do
--     ptrMap <- State.execDefT @Component.PointerMap
--         $! MemoryRegion.withRaw memReg
--         $! buildClusterSerializer @comps @comps clusters
--     pure $ SerializeInfo memReg $ unwrap ptrMap
-- {-# INLINE serializeClusters #-}
