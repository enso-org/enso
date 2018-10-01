{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Fold.Partition where

import Prologue

import qualified Data.Graph.Data.Component.Class as Component
import qualified Data.Graph.Data.Component.List  as ComponentList
import qualified Data.Graph.Data.Graph.Class     as Graph
import qualified Data.Graph.Data.Layer.Layout    as Layout
import qualified Data.Graph.Fold.Class           as Fold
import qualified Data.Graph.Fold.Deep            as Deep
import qualified Data.Graph.Fold.Scoped          as Fold
import qualified Data.TypeMap.Strict             as TypeMap

import Data.Graph.Component.Edge       (Target)
import Data.Graph.Component.Node.Layer (Users)
import Data.Graph.Data.Component.Class (Component)
import Data.Graph.Data.Component.List  (ComponentList, ComponentLists)
import Data.TypeMap.Strict             (TypeMap)

import           Data.Set (Set)
import qualified Data.Set as Set


-- TODO
-- This implementation uses now Set to store components, however, we do not
-- check if a component was already traversed and we traverse components many
-- times if they build a tree which branches and joins. We should use the
-- information in sets to make it more efficient.

type ComponentSet comp = Set (Component.Some comp)

type family ComponentSets comps where
    ComponentSets '[] = '[]
    ComponentSets (c ': cs) = (ComponentSet c ': ComponentSets cs)


----------------------
-- === Clusters === --
----------------------

-- === Definitions === --

type    Clusters__ comps = TypeMap (ComponentSets comps)
newtype Clusters   comps = Clusters (Clusters__ comps)

makeLenses ''Clusters


-- === API === --

type SplitHead comp comps
   = TypeMap.SplitHead (ComponentSet comp) (ComponentSets comps)

splitHead :: SplitHead comp comps
    => Clusters (comp ': comps) -> (ComponentSet comp, Clusters comps)
splitHead = fmap wrap . TypeMap.splitHead . unwrap
{-# INLINE splitHead #-}


-- === Instances === --

deriving instance Mempty (Clusters__ comps) => Mempty (Clusters comps)
deriving instance Show   (Clusters__ comps) => Show   (Clusters comps)



-------------------------------
-- === Cluster Discovery === --
-------------------------------

-- === Definition === --

type ClustersM  m     = Clusters  (Graph.ComponentsM m)
type DiscoveryM m     = Discovery (Graph.ComponentsM m)

data Discovery  (comps :: [Type]) deriving (Generic)
type instance Fold.Result     (Discovery comps) = Clusters comps
type instance Fold.LayerScope (Discovery comps)
   = 'Fold.Blacklist '[Target, Users]

type ClusterEditor t ts = TypeMap.ElemEditor (ComponentSet  t)
                                             (ComponentSets ts)


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

instance (MonadIO m, ClusterEditor comp comps)
    => Fold.ComponentBuilder (Discovery comps) m comp where
    componentBuild = \comp acc -> (
          wrap
        . TypeMap.modifyElem_ @(ComponentSet comp)
          (Set.insert $ Layout.relayout comp)
        . unwrap
      <$> acc)
    {-# INLINE componentBuild #-}


