{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Serialize.Internal where

import Prologue

import qualified Control.Monad.State.Layered       as State
import qualified Data.Graph.Data.Component.List    as Component
import qualified Data.Graph.Fold.Partition         as Partition
import qualified Data.Graph.Serialize.Alloc        as Alloc
import qualified Data.Graph.Serialize.Component    as Component
import qualified Data.Graph.Serialize.MemoryRegion as MemoryRegion
import qualified Data.TypeMap.Strict               as TypeMap

import Data.Graph.Serialize.Component    (ExternalStorableComponent,
                                          ExternalStorableComponents)
import Data.Graph.Serialize.MemoryRegion (MemoryRegion, RawMemoryRegion)
import Data.Map                          (Map)
import Foreign.Ptr.Utils                 (SomePtr)



---------------------------------------
-- === Serialization of Clusters === --
---------------------------------------

-- === Iterate over Cluster and serialize each list === --

class ClusterSerializer' (cs :: [Type]) comps m where
    serializeClusters' :: Partition.Clusters comps -> m RawMemoryRegion -> m RawMemoryRegion

instance ClusterSerializer' '[] ts m where
    serializeClusters' = \_ -> id   ; {-# INLINE serializeClusters' #-}

instance
    ( TypeMap.ElemGetter (Component.List comp) (Component.Lists comps)
    , ExternalStorableComponent comp m
    , ClusterSerializer' cs comps m
    , State.Monad Component.PointerMap m
    , MonadIO m
    ) => ClusterSerializer' (comp ': cs) comps m where
    serializeClusters' clusters accM = do
        let compList = TypeMap.getElem @(Component.List comp) clusters
            acc'     = Component.dumpComponentList compList accM
        serializeClusters' @cs @comps clusters acc'
    {-# INLINE serializeClusters' #-}


-- === API === --

data SerializeInfo = SerializeInfo
    { _memoryRegion :: !MemoryRegion
    , _ptrMapping   :: !(Map SomePtr SomePtr)
    }
makeLenses ''SerializeInfo

type ClusterSerializer comps m =
    ( Alloc.Allocator comps m
    , ExternalStorableComponents comps m
    , ClusterSerializer' comps comps (State.StateT Component.PointerMap m)
    )

serializeClusters :: ∀ comps m. ClusterSerializer comps m
                  => Partition.Clusters comps -> MemoryRegion -> m SerializeInfo
serializeClusters clusters memReg = do
    ptrMap <- State.execDefT @Component.PointerMap $!
        MemoryRegion.withRaw memReg $ \rawMemReg ->
            serializeClusters' @comps @comps clusters $ pure rawMemReg
    pure $! SerializeInfo memReg $! unwrap ptrMap
{-# INLINE serializeClusters #-}
