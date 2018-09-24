{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Fold.SubTree where

import Prologue

import qualified Control.Monad.State.Layered          as State
import qualified Data.Generics.Traversable            as GTraversable
import qualified Data.Graph.Component.Edge            as Edge
import qualified Data.Graph.Component.Node.Class      as Node
import qualified Data.Graph.Component.Node.Layer.Type as Type
import qualified Data.Graph.Data.Component.Class      as Component
import qualified Data.Graph.Data.Component.List       as ComponentList
import qualified Data.Graph.Data.Graph.Class          as Graph
import qualified Data.Graph.Data.Layer.Class          as Layer
import qualified Data.Graph.Data.Layer.Layout         as Layout
import qualified Data.Graph.Fold.Class                as Fold
import qualified Data.Graph.Fold.Deep                 as Deep
import qualified Data.Graph.Fold.Scoped               as Fold
import qualified Data.Map.Strict                      as Map
import qualified Foreign.Ptr                          as Ptr
import qualified Foreign.Storable                     as Storable
import qualified Type.Data.List                       as List

import Data.Generics.Traversable             (GTraversable)
import Data.Graph.Component.Edge.Class       (Source)
import Data.Graph.Component.Node.Layer.Model (Model)
import Data.Graph.Data.Component.Class       (Component)
import Data.Graph.Data.Component.List        (SomeComponentList)
import Data.Vector.Storable.Foreign          (Vector)
import Foreign.Ptr.Utils                     (SomePtr)
import Type.Data.Bool                        (Not, type (||))



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
