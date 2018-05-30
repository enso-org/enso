{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Serialize.Component where

import Prologue

import qualified Control.Monad.State.Layered       as State
import qualified Data.Graph.Data.Component.Class   as Component
import qualified Data.Graph.Data.Component.List    as Component
import qualified Data.Graph.Data.Graph.Class       as Graph
import qualified Data.Graph.Data.Layer.Class       as Layer
import qualified Data.Graph.Fold.Partition         as Partition
import qualified Data.Graph.Serialize.Alloc        as Alloc
import qualified Data.Graph.Serialize.MemoryRegion as MemoryRegion
import qualified Data.Graph.Storable.External      as ExternalStorable
import qualified Data.Map                          as Map
import qualified Data.TypeMap.Strict               as TypeMap
import qualified Foreign.Storable.Utils            as Storable

import Data.Graph.Data.Component.Class   (Component)
import Data.Graph.Serialize.MemoryRegion (MemoryRegion,
                                          RawMemoryRegion (RawMemoryRegion))
import Data.Graph.Storable.External      (ExternalStorable)
import Data.Map                          (Map)
import Foreign.Ptr                       (Ptr, plusPtr)
import Foreign.Ptr.Utils                 (SomePtr)
import Foreign.Storable.Utils            (Storable)
import Foreign.Storable1                 (Storable1)


------------------------------------
-- === Serializing components === --
------------------------------------

-- === Helpers === --

newtype PointerMap = PointerMap (Map SomePtr SomePtr) deriving Default
makeLenses ''PointerMap

recordMapping :: State.Monad PointerMap m => SomePtr -> SomePtr -> m ()
recordMapping = \oldPtr newPtr ->
    State.modify_ @PointerMap (wrap . Map.insert oldPtr newPtr . unwrap)


-- === Serializing layers === --

class ExternalStorableLayers (layers :: [Type]) where
    dumpLayersBuilder :: SomePtr -> IO SomePtr -> IO SomePtr

instance ExternalStorableLayers '[] where
    dumpLayersBuilder = \_ -> id
    {-# INLINE dumpLayersBuilder #-}

instance ( ExternalStorableLayers ls
         , ExternalStorable (Layer.Cons l ()) -- FIXME
         , Storable1 (Layer.Cons l) )
       => ExternalStorableLayers (l ': ls) where
    dumpLayersBuilder ptr mdynPtr = dumpLayersBuilder @ls ptr' mdynPtr' where
        ptr'     = ptr `plusPtr` Layer.byteSize @l
        mdynPtr' = ExternalStorable.dumpBuilder
                   (coerce ptr :: Ptr (Layer.Cons l ())) mdynPtr
    {-# INLINE dumpLayersBuilder #-}

dumpLayers :: ∀ layers. ExternalStorableLayers layers
           => SomePtr -> SomePtr -> IO SomePtr
dumpLayers = \ptr -> dumpLayersBuilder @layers ptr . pure
{-# INLINE dumpLayers #-}


-- === Serializing components === --

type ExternalStorableComponent comp m =
    ( MonadIO m
    , ExternalStorableLayers (Graph.DiscoverComponentLayers m comp)
    )

type family ExternalStorableComponents comps m :: Constraint  where
    ExternalStorableComponents '[] m = ()
    ExternalStorableComponents (c ': cs) m =
        (ExternalStorableComponent c m, ExternalStorableComponents cs m)

dumpComponent :: ∀ comp m layout. ExternalStorableComponent comp m
              => Component comp layout -> SomePtr -> m SomePtr
dumpComponent = \comp dynPtr -> liftIO
    $! dumpLayers @(Graph.DiscoverComponentLayers m comp) (coerce comp) dynPtr
{-# INLINE dumpComponent #-}

dumpComponentToMemRegion :: ∀ comp m.
    ( ExternalStorableComponent comp m
    , Storable (Component.Some comp)
    , State.Monad PointerMap m
    ) => RawMemoryRegion -> Component.Some comp -> m RawMemoryRegion
dumpComponentToMemRegion = \(RawMemoryRegion !staticPtr !dynamicPtr) comp -> do
    staticPtr'  <- Storable.castPokeAndOffset staticPtr comp
    dynamicPtr' <- dumpComponent comp dynamicPtr
    recordMapping staticPtr staticPtr'
    pure $! RawMemoryRegion staticPtr' dynamicPtr'
{-# INLINE dumpComponentToMemRegion #-}


-- === Serializing component lists === --

dumpComponentList :: ∀ comp m.
    ( ExternalStorableComponent comp m
    , State.Monad PointerMap m
    ) =>  Component.List comp -> m RawMemoryRegion -> m RawMemoryRegion
dumpComponentList = \compList memRegM -> do
    memReg <- memRegM
    foldM dumpComponentToMemRegion memReg $! toList compList
{-# INLINE dumpComponentList #-}
