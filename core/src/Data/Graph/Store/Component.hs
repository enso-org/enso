{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Store.Component where

-- import qualified Control.Monad.State.Layered     as State
-- import qualified Data.Graph.Data.Component.Class as Component
-- import qualified Data.Graph.Data.Component.List  as ComponentList
-- import qualified Data.Graph.Data.Graph.Class     as Graph
-- import qualified Data.Graph.Data.Layer.Class     as Layer
-- import qualified Data.Graph.Fold.Partition       as Partition
-- import qualified Data.Graph.Store.Alloc          as Alloc
-- import qualified Data.Graph.Store.External       as ExternalStorable
-- import qualified Data.Graph.Store.MemoryRegion   as MemoryRegion
-- import qualified Data.Map                        as Map
-- import qualified Data.TypeMap.Strict             as TypeMap
-- import qualified Foreign.Storable.Utils          as Storable

-- import Data.Graph.Data.Component.Class (Component)
-- import Data.Graph.Data.Component.List  (ComponentList)
-- import Data.Graph.Store.External       (ExternalStorable)
-- import Data.Graph.Store.MemoryRegion   (MemoryRegion)
-- import Data.Map                        (Map)
-- import Foreign.Ptr                     (Ptr, plusPtr)
-- import Foreign.Ptr.Utils               (SomePtr)
-- import Foreign.Storable.Utils          (Storable)
-- import Foreign.Storable1               (Storable1)



-- ------------------------------------
-- -- === Serializing components === --
-- ------------------------------------

-- -- === Helpers === --

-- newtype PointerMap = PointerMap (Map SomePtr SomePtr) deriving Default
-- makeLenses ''PointerMap

-- recordMapping :: State.Monad PointerMap m => SomePtr -> SomePtr -> m ()
-- recordMapping = \oldPtr newPtr -> State.modify_ @PointerMap
--                                 $ wrapped %~ Map.insert oldPtr newPtr
-- {-# INLINE recordMapping #-}


-- -- === Serializing layers === --

-- class ExternalStorableLayers (layers :: [Type]) where
--     dumpLayersBuilder :: SomePtr -> IO MemoryRegion.Dynamic -> IO MemoryRegion.Dynamic

-- instance ExternalStorableLayers '[] where
--     dumpLayersBuilder = \_ -> id
--     {-# INLINE dumpLayersBuilder #-}

-- instance ( ExternalStorableLayers ls
--          , ExternalStorable (Layer.Cons l ()) -- FIXME
--          , Storable1 (Layer.Cons l) )
--        => ExternalStorableLayers (l ': ls) where
--     dumpLayersBuilder ptr mdynPtr = dumpLayersBuilder @ls ptr' mdynPtr' where
--         ptr'     = ptr `plusPtr` Layer.byteSize @l
--         mdynPtr' = ExternalStorable.dumpBuilder
--                    (coerce ptr :: Ptr (Layer.Cons l ())) mdynPtr
--     {-# INLINE dumpLayersBuilder #-}

-- dumpLayers :: ∀ layers. ExternalStorableLayers layers
--            => SomePtr -> MemoryRegion.Dynamic -> IO MemoryRegion.Dynamic
-- dumpLayers = \ptr -> dumpLayersBuilder @layers ptr . pure
-- {-# INLINE dumpLayers #-}


-- -- === Serializing components === --

-- type ExternalStorableComponent comp m =
--     ( MonadIO m
--     , ExternalStorableLayers (Graph.DiscoverComponentLayers m comp)
--     )

-- type family ExternalStorableComponents comps m :: Constraint where
--     ExternalStorableComponents '[] m = ()
--     ExternalStorableComponents (c ': cs) m =
--         (ExternalStorableComponent c m, ExternalStorableComponents cs m)

-- dumpComponent :: ∀ comp m layout. ExternalStorableComponent comp m
--               => Component comp layout -> MemoryRegion.Dynamic -> m MemoryRegion.Dynamic
-- dumpComponent = \comp dynPtr -> liftIO
--     $! dumpLayers @(Graph.DiscoverComponentLayers m comp) (coerce comp) dynPtr
-- {-# INLINE dumpComponent #-}

-- dumpComponentToMemRegion :: ∀ comp m.
--     ( ExternalStorableComponent comp m
--     , Storable (Component.Some comp)
--     , State.Monad PointerMap m
--     ) => MemoryRegion.Raw -> Component.Some comp -> m MemoryRegion.Raw
-- dumpComponentToMemRegion = \memReg comp -> do
--     let staticPtr  = memReg ^. MemoryRegion.staticMemPtr
--         dynamicReg = MemoryRegion.viewDynamic memReg
--     staticPtr'  <- Storable.castPokeAndOffset staticPtr comp
--     dynamicReg' <- dumpComponent comp dynamicReg
--     recordMapping staticPtr staticPtr'
--     pure $! MemoryRegion.constructRaw dynamicReg' staticPtr'
-- {-# INLINE dumpComponentToMemRegion #-}


-- -- === Serializing component lists === --

-- dumpComponentList :: ∀ comp m.
--     ( ExternalStorableComponent comp m
--     , State.Monad PointerMap m
--     ) => ComponentList comp -> MemoryRegion.Raw -> m MemoryRegion.Raw
-- dumpComponentList = \compList memReg -> do
--     ComponentList.foldlM dumpComponentToMemRegion memReg compList
-- {-# INLINE dumpComponentList #-}
