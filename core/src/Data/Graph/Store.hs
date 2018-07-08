module Data.Graph.Store where

import Prologue hiding (pprint, print, putStrLn)

import qualified Data.Graph.Component.Node.Class as Component
import qualified Data.Graph.Data.Component.Class as Component
import qualified Data.Graph.Data.Graph.Class     as Graph
import qualified Data.Graph.Fold.Partition       as Partition
import qualified Data.Graph.Store.Alloc          as Alloc
import qualified Data.Graph.Store.Buffer         as Buffer
import qualified Data.Graph.Store.Internal       as Serialize
import qualified Data.Graph.Store.Size.Discovery as Size
import qualified Data.Map.Strict                 as Map
import qualified Data.Mutable.Class              as Mutable
import qualified Memory                          as Memory

import Data.ByteString                 (ByteString)
import Data.Graph.Data.Component.Class (Component)
import Data.Graph.Data.Component.Set   (ComponentSet, ComponentSetA)
import Foreign.Ptr.Utils               (SomePtr)

-- import Data.Graph.Store.MemoryRegion   (MemoryRegion)


putStrLn :: Applicative m => String -> m ()
putStrLn = const $ pure ()

print :: Applicative m => a -> m ()
print = const $ pure ()

pprint :: Applicative m => a -> m ()
pprint = const $ pure ()


-- -----------------------
-- -- === Discovery === --
-- -----------------------

-- -- === Definition === --

-- data Dump (scope :: Fold.Scope)
-- type instance Fold.Result     (Dump scope) = Component.List'
-- type instance Fold.LayerScope (Dump scope) = scope


-- -- === API === --

-- type Dumpable  scope = Deep.Builder  (Dump scope)
-- type Dumpable1 scope = Deep.Builder1 (Dump scope)

-- dump  :: ∀ scope m a.   Dumpable  scope m a => a   -> m Component.List'
-- dump1 :: ∀ scope m a t. Dumpable1 scope m a => a t -> m Component.List'
-- dump  = Deep.run  @(Dump scope)
-- dump1 = Deep.run1 @(Dump scope)
-- {-# INLINE dump #-}
-- {-# INLINE dump1 #-}


-- -- === Simple === --

-- -- type SimpleDiscoveryScope = 'Fold.Whitelist '[Model, Type.Type, Source]
-- -- type Dumpable'   = Dumpable  SimpleDiscoveryScope
-- -- type Dumpable1'  = Dumpable1 SimpleDiscoveryScope

-- -- dump'  :: ∀ m a.   Dumpable'  m a => a   -> m Component.List'
-- -- dump1' :: ∀ m a t. Dumpable1' m a => a t -> m Component.List'
-- -- dump'  = dump  @SimpleDiscoveryScope
-- -- dump1' = dump1 @SimpleDiscoveryScope
-- -- {-# INLINE dump'  #-}
-- -- {-# INLINE dump1' #-}


-- -- === Instances === --

-- instance Monad m => Fold.LayerBuilder (Dump scope) m layer where
--     layerBuild = \layer macc -> do

--         (Component.Cons $ Layout.relayout cmp) <$> acc
--     {-# INLINE componentBuild #-}



------------------------------------
-- === SubGraph serialization === --
------------------------------------

type Serializer comp m =
    ( MonadIO m
    , Partition.Partition comp m
    , Size.ClusterSizeDiscovery (Graph.ComponentsM m) m
    , Size.ClusterSizeCount     (Graph.ComponentsM m) m
    , Buffer.Alloc m

    , Buffer.StaticRegionEncoder (Graph.ComponentsM m) m
    , Buffer.ComponentStaticInitializer m
    , Buffer.ComponentStaticRedirection (Graph.ComponentsM m) m
    , Buffer.ComponentUnswizzle__ (Graph.ComponentsM m) m

    , Show (Partition.ClustersM m)

    -- , Alloc.Allocator comps m
    -- , Serialize.ClusterSerializer comps m
    )

serialize :: ∀ comp m layout. Serializer Component.Nodes m
    => Component.Node layout -> m ByteString -- MemoryRegion
serialize comp = do
    putStrLn "\nSERIALIZE"
    putStrLn $ "root = " <> show comp

    clusters  <- Partition.partition comp
    size      <- Size.clusterSize clusters
    ccount    <- Size.componentCount clusters
    buffer    <- Buffer.alloc ccount size

    let dataRegion    = Buffer.dataRegion buffer
    let dataRegionPtr = unwrap dataRegion
    dynDataRegion <- Buffer.dynDataRegion buffer

    putStrLn "\n=== clusters ===" >> pprint clusters

    putStrLn $ "\nsize = " <> show size
    putStrLn $ "\nccount = " <> show ccount

    putStrLn "\n=== encodeStaticRegion ==="
    redirectMap <- Memory.withUnmanagedPtr dataRegionPtr $ Buffer.encodeStaticRegion comp clusters

    putStrLn "\n=== redirectMap ==="
    pprint redirectMap

    putStrLn "\n=== copyInitializeComps ==="
    Buffer.copyInitializeComponents ccount buffer -- dataRegion dynDataRegion

    putStrLn "\n=== pointerRedirection ==="
    Buffer.redirectComponents @(Graph.ComponentsM m) redirectMap ccount dataRegion

    -- FIXME FIXME FIXME
    -- FIXME FIXME FIXME
    -- FIXME FIXME FIXME
    -- Unswizzling is disabled for now. Enable it and create swizzling!
    -- putStrLn "\n=== pointer unswizzling ==="
    -- Buffer.unswizzleComponents__ @(Graph.ComponentsM m) ccount dataRegion

    Buffer.unsafeFreeze buffer
{-# INLINE serialize #-}


type Deserializer m =
    ( MonadIO m
    , Buffer.ComponentCountDecoder m
    , Buffer.StaticComponentsDecoder m
    , Buffer.ComponentStaticInitializer2 m
    , Buffer.ComponentStaticRedirection2 (Graph.ComponentsM m) m
    )

deserialize :: ∀ comp layout m. Deserializer m => ByteString -> m (Component comp layout)
deserialize = \bs -> do
    buffer <- Buffer.unsafeThaw bs

    putStrLn "\n=== decodeComponentCount ==="
    ccount <- Buffer.decodeComponentCount buffer

    putStrLn "\n=== decodeStaticComponents ==="
    offsetMap <- Buffer.decodeStaticComponents ccount buffer

    putStrLn "\n=== offsetMap ==="
    pprint offsetMap

    putStrLn "\n=== copyInitializeComps2 ==="
    let (offs, cmpPtrs) = offsetMap
    Buffer.copyInitializeComponents2 ccount cmpPtrs -- dataRegion dynDataRegion

    putStrLn "\n=== pointerRedirection ==="
    Buffer.redirectComponents2 @(Graph.ComponentsM m) offs ccount cmpPtrs

    let (p:_) = cmpPtrs
    pure $ Component.unsafeFromPtr (unwrap p)

    -- putStrLn "\n=== copyInitializeComps2 ==="
    -- Buffer.copyInitializeComps2 ccount buffer

    -- pure ()
