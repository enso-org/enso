{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Traversal.SubTree where

import Prologue hiding (Traversable, Traversal, Traversal', fold, fold1,
                 traverse)

import qualified Control.Monad.State.Layered          as State
import qualified Data.Generics.Traversable            as GTraversable
import qualified Data.Graph.Component.Edge            as Edge
import qualified Data.Graph.Component.Node.Class      as Node
import qualified Data.Graph.Component.Node.Layer.Type as Type
import qualified Data.Graph.Data.Component.Class      as Component
import qualified Data.Graph.Data.Graph.Class          as Graph
import qualified Data.Graph.Data.Layer.Class          as Layer
import qualified Data.Graph.Data.Layer.Layout         as Layout
import qualified Data.Graph.Traversal.Deep            as Deep
import qualified Data.Graph.Traversal.Fold            as Fold
import qualified Data.Graph.Traversal.Scoped          as Fold
import qualified Data.Map.Strict                      as Map
import qualified Foreign.Ptr                          as Ptr
import qualified Foreign.Storable                     as Storable
import qualified Type.Data.List                       as List

import Data.Generics.Traversable             (GTraversable)
import Data.Graph.Component.Edge.Class       (Source)
import Data.Graph.Component.Node.Layer.Model (Model)
import Data.Graph.Data.Component.Class       (Component)
import Data.PtrList.Mutable                  (UnmanagedPtrList)
import Data.Vector.Storable.Foreign          (Vector)
import Foreign.Ptr.Utils                     (SomePtr)
import Type.Data.Bool                        (Not, type (||))


-----------------------
-- === Discovery === --
-----------------------

-- === Definition === --

data Discovery (scope :: Fold.Scope)
type instance Fold.Result     (Discovery scope) = [Component.Any]
type instance Fold.LayerScope (Discovery scope) = scope


-- === API === --

type SubTree  scope = Deep.Builder  (Discovery scope)
type SubTree1 scope = Deep.Builder1 (Discovery scope)

subTree :: ∀ scope m a. SubTree scope m a => a -> m [Component.Any]
subTree = \a -> Deep.build @(Discovery scope) a $! pure mempty
{-# INLINE subTree #-}


-- === Simple === --

type SimpleDiscoveryScope = 'Fold.Whitelist '[Model, Type.Type, Source]
type Discovery' = Discovery SimpleDiscoveryScope
type SubTree'   = SubTree   SimpleDiscoveryScope
type SubTree1'  = SubTree1  SimpleDiscoveryScope

subTree' :: ∀ m a. SubTree' m a => a -> m [Component.Any]
subTree' = subTree @SimpleDiscoveryScope
{-# INLINE subTree' #-}


-- === Instances === --

instance Monad m => Fold.ComponentBuilder (Discovery scope) m tag where
    componentBuild = \comp acc -> (Layout.relayout comp :) <$> acc
    {-# INLINE componentBuild #-}
