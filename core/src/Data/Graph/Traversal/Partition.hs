{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Traversal.Partition where

import Prologue hiding (Traversal, empty)

import qualified Data.Graph.Component.Edge            as Edge
import qualified Data.Graph.Component.Node.Class      as Node
import qualified Data.Graph.Component.Node.Layer.Type as Type
import qualified Data.Graph.Data.Component.Class      as Component
import qualified Data.Graph.Data.Component.List       as Component
import qualified Data.Graph.Data.Graph.Class          as Graph
import qualified Data.Graph.Data.Layer.Class          as Layer
import qualified Data.Graph.Data.Layer.Layout         as Layout
import qualified Data.Graph.Traversal.Fold            as Fold
import qualified Data.Graph.Traversal.Scoped          as Fold
import qualified Data.Set                             as Set
import qualified Data.Set.Mutable.Class               as MutableSet
import qualified Data.TypeMap.Strict                  as TypeMap

import Data.Graph.Component.Edge       (Target)
import Data.Graph.Component.Node.Layer (Users)
import Data.Graph.Data.Component.Class (Component)
import Data.TypeMap.Strict             (TypeMap)



-------------------------------
-- === Cluster Discovery === --
-------------------------------

-- === Datatypes and aliases === --

data Discovery (comps :: [Type])
type Clusters comps = TypeMap (Component.Lists comps)
type instance Fold.Result     (Discovery comps) = Clusters comps
type instance Fold.LayerScope (Discovery comps) = 'Fold.Blacklist '[Target, Users]

type Traversal     comps = Fold.Builder  (Fold.Scoped (Discovery comps))
type Traversal1    comps = Fold.Builder1 (Fold.Scoped (Discovery comps))
type ClusterEditor t ts  = TypeMap.ElemEditor (Component.List  t)
                                              (Component.Lists ts)


-- === API === --

empty :: ∀ comps. Default (Clusters comps) => Clusters comps
empty = def ; {-# INLINE empty #-}

type Partition tag m =
    ( Traversal1 (Graph.DiscoverComponents m) m (Component tag)
    , Default (Clusters (Graph.DiscoverComponents m))
    )

partition :: ∀ tag layout m comps.
    ( Partition tag m
    , comps ~ Graph.DiscoverComponents m
    ) => Component tag layout -> m (Clusters comps)
partition = \comp
    -> Fold.build1 @(Fold.Scoped (Discovery comps)) comp (pure $! empty @comps)
{-# INLINE partition #-}


-- === Instances === --

instance (Monad m, ClusterEditor comp comps)
    => Fold.ComponentBuilder (Discovery comps) m comp where
    componentBuild = \comp acc -> do
        typeTpl <- acc
        let typeTpl' = TypeMap.modifyElem_ @(Component.List comp)
                       (Component.Cons $ Layout.relayout comp) typeTpl
        pure $! typeTpl'
    {-# INLINE componentBuild #-}

instance ( Monad m
         , Traversal comps m Node.Some
         , ClusterEditor Edge.Edges comps
         , Layer.Reader Edge.Edge Edge.Source m
         , Layer.Reader Edge.Edge Edge.Target m
         )
    => Fold.LayerBuilder (Discovery comps) m Type.Type where
    layerBuild = \tpLink acc -> do
        (tp  :: Node.Some) <- Layout.relayout <$> Layer.read @Edge.Source tpLink
        (tgt :: Node.Some) <- Layout.relayout <$> Layer.read @Edge.Target tpLink
        let acc' = Fold.componentBuild @(Discovery comps) tpLink acc
        if tp == tgt
            then acc'
            else Fold.build @(Fold.Scoped (Discovery comps)) tp acc'
    {-# INLINE layerBuild #-}

instance {-# OVERLAPPABLE #-} (Monad m, Traversal1 comps m (Layer.Cons layer))
    => Fold.LayerBuilder (Discovery comps) m layer where
    layerBuild = Fold.build1 @(Fold.Scoped (Discovery comps))
    {-# INLINE layerBuild #-}



-- ===================================================
-- NOTE[piotrMocz] once we start needing cpp struct traversals, this
-- code will handle Cpp Sets.
-- ===================================================
-- uniqueNodesFromSet ::
--     ( MonadIO m
--     , Layer.Reader Edge.Edge Edge.Source m
--     , Layer.Reader Edge.Edge Edge.Target m
--     ) => Edge.Set comp -> m [Node.Some]
-- uniqueNodesFromSet = \edgeSet -> do
--     !edges <- MutableSet.toList edgeSet
--     let processEdge = \s e -> do
--             (src :: Node.Some) <- Layout.relayout <$> Layer.read @Edge.Source e
--             (tgt :: Node.Some) <- Layout.relayout <$> Layer.read @Edge.Source e
--             pure . Set.insert src $ Set.insert tgt s
--     Set.toList <$> foldM processEdge mempty edges
-- {-# INLINE uniqueNodesFromSet #-}

-- instance ( MonadIO m
--          , Layer.Reader Edge.Edge Edge.Source m
--          , Layer.Reader Edge.Edge Edge.Target m
--          , ClusterEditor Node.Nodes comps
--          ) => Fold.Builder1 (Fold.Scoped (Discovery comps)) m Edge.Set where
--     build1 = \edgeSet acc -> do
--         comps   <- uniqueNodesFromSet edgeSet
--         typeTpl <- acc
--         let typeTpl' = TypeMap.modifyElem_ @(Component.List Node.Nodes)
--                        (comps <>) typeTpl
--         pure $ typeTpl'
--     {-# INLINE build1 #-}

