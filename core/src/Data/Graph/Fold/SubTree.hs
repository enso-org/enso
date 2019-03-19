{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Fold.SubTree where

import Prologue

import qualified Data.Graph.Component.Node.Layer.Type as Type
import qualified Data.Graph.Data.Component.List       as ComponentList
import qualified Data.Graph.Data.Layer.Layout         as Layout
import qualified Data.Graph.Fold.Class                as Fold
import qualified Data.Graph.Fold.Deep                 as Deep
import qualified Data.Graph.Fold.Scoped               as Fold

import Data.Graph.Component.Edge.Class       (Source)
import Data.Graph.Component.Node.Layer.Model (Model)
import Data.Graph.Data.Component.List        (SomeComponentList)



-----------------------
-- === Discovery === --
-----------------------

-- === Definition === --

data Discovery (scope :: Fold.Scope) deriving (Generic)
type instance Fold.Result     (Discovery scope) = SomeComponentList
type instance Fold.LayerScope (Discovery scope) = scope


-- === API === --

type SubTree  scope = Deep.Builder  (Discovery scope)
type SubTree1 scope = Deep.Builder1 (Discovery scope)

subTree  :: ∀ scope m a.   SubTree  scope m a => a   -> m SomeComponentList
subTree1 :: ∀ scope m a t. SubTree1 scope m a => a t -> m SomeComponentList
subTree  = Deep.run  @(Discovery scope)
subTree1 = Deep.run1 @(Discovery scope)
{-# INLINE subTree #-}
{-# INLINE subTree1 #-}


-- === Simple === --

type SimpleDiscoveryScope = 'Fold.Whitelist '[Model, Type.Type, Source]
type SubTree'   = SubTree  SimpleDiscoveryScope
type SubTree1'  = SubTree1 SimpleDiscoveryScope

subTree'  :: ∀ m a.   SubTree'  m a => a   -> m SomeComponentList
subTree1' :: ∀ m a t. SubTree1' m a => a t -> m SomeComponentList
subTree'  = subTree  @SimpleDiscoveryScope
subTree1' = subTree1 @SimpleDiscoveryScope
{-# INLINE subTree'  #-}
{-# INLINE subTree1' #-}


-- === Instances === --

instance MonadIO m => Fold.ComponentBuilder (Discovery scope) m tag where
    componentBuild = \cmp acc
        -> (ComponentList.Cons $ Layout.relayout cmp) <$> acc
    {-# INLINE componentBuild #-}
