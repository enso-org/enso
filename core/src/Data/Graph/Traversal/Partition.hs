{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Traversal.Partition where

import Prologue

import qualified Data.Graph.Component.Edge            as Edge
import qualified Data.Graph.Component.Node.Class      as Node
import qualified Data.Graph.Component.Node.Layer.Type as Type
import qualified Data.Graph.Data.Component.Class      as Component
import qualified Data.Graph.Data.Component.List       as Component
import qualified Data.Graph.Data.Graph.Class          as Graph
import qualified Data.Graph.Data.Layer.Class          as Layer
import qualified Data.Graph.Data.Layer.Layout         as Layout
import qualified Data.Graph.Traversal.Deep            as Deep
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

type Clusters   comps = TypeMap   (Component.Lists comps)
type ClustersM  m     = Clusters  (Graph.DiscoverComponents m)
type DiscoveryM m     = Discovery (Graph.DiscoverComponents m)

data Discovery  (comps :: [Type])
type instance Fold.Result     (Discovery comps) = Clusters comps
type instance Fold.LayerScope (Discovery comps)
   = 'Fold.Blacklist '[Target, Users]

type ClusterEditor t ts = TypeMap.ElemEditor (Component.List  t)
                                             (Component.Lists ts)


-- === API === --

type Partition comp m =
    ( Deep.Builder1 (DiscoveryM m) m (Component comp)
    , Mempty (ClustersM m)
    )

partition :: ∀ comp m layout. Partition comp m
          => Component comp layout -> m (ClustersM m)
partition = Deep.run1 @(DiscoveryM m)
{-# INLINE partition #-}


-- === Instances === --

instance (Monad m, ClusterEditor comp comps)
    => Fold.ComponentBuilder (Discovery comps) m comp where
    componentBuild = \comp acc -> TypeMap.modifyElem_ @(Component.List comp)
                                  (Component.Cons $ Layout.relayout comp)
                              <$> acc
    {-# INLINE componentBuild #-}


