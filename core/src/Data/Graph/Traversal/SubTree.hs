-- {-# LANGUAGE Strict               #-}
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
import qualified Data.Graph.Data.Component.Set        as Setx
import qualified Data.Graph.Data.Graph.Class          as Graph
import qualified Data.Graph.Data.Layer.Class          as Layer
import qualified Data.Graph.Data.Layer.Layout         as Layout
import qualified Data.Graph.Traversal.Fold            as Fold
import qualified Data.Graph.Traversal.Scoped          as Fold
import qualified Data.Map.Strict                      as Map
import qualified Data.Set                             as Set
import qualified Foreign.Ptr                          as Ptr
import qualified Foreign.Storable                     as Storable
import qualified Type.Data.List                       as List

import Data.Generics.Traversable             (GTraversable)
import Data.Graph.Component.Edge.Class       (Source)
import Data.Graph.Component.Node.Layer.Model (Model)
import Data.Graph.Data.Component.Class       (Component)
import Data.PtrList.Mutable                  (UnmanagedPtrList)
import Data.Set                              (Set)
import Data.Vector.Storable.Foreign          (Vector)
import Foreign.Ptr.Utils                     (SomePtr)
import Type.Data.Bool                        (Not, type (||))


-----------------------
-- === Discovery === --
-----------------------

-- === Definition === --

data Discovery (edges :: [Type])
type instance Fold.Result     (Discovery es) = [Component.Any]
type instance Fold.LayerScope (Discovery es) = 'Fold.Whitelist es


-- === API === --

type SubTree  es = Fold.Builder  (Fold.Scoped (Discovery es))
type SubTree1 es = Fold.Builder1 (Fold.Scoped (Discovery es))

subTree :: ∀ es m a. SubTree es m a => a -> m [Component.Any]
subTree = \a -> Fold.build @(Fold.Scoped (Discovery es)) a $! pure mempty
{-# INLINE subTree #-}


-- === Simple === --

type SimpleDiscoveryTarget = '[Model, Type.Type, Source]
type Discovery'  = Discovery SimpleDiscoveryTarget
type SubTree'  = Fold.Builder  (Fold.Scoped Discovery')
type SubTree1' = Fold.Builder1 (Fold.Scoped Discovery')

subTree' :: ∀ m a. SubTree' m a => a -> m [Component.Any]
subTree' = subTree @SimpleDiscoveryTarget
{-# INLINE subTree' #-}


-- === Instances === --

instance Monad m
      => Fold.ComponentBuilder (Discovery es) m tag where
    componentBuild = \comp acc -> do
        a <- acc
        pure $! Layout.relayout comp : a
    {-# INLINE componentBuild #-}

instance ( MonadIO m
         , SubTree es m Node.Some
         , Layer.Reader Edge.Edge Edge.Source m
         , Layer.Reader Edge.Edge Edge.Target m
         )
      => Fold.LayerBuilder (Discovery es) m Type.Type where
    layerBuild = \tpLink acc -> do
        (tp  :: Node.Some) <- Layout.relayout <$> Layer.read @Edge.Source tpLink
        (tgt :: Node.Some) <- Layout.relayout <$> Layer.read @Edge.Target tpLink
        a <- acc
        let acc' = pure $! Layout.relayout tpLink : a
        if tp == tgt then acc'
                     else Fold.build @(Fold.Scoped (Discovery es)) tp acc'
    {-# INLINE layerBuild #-}


instance {-# OVERLAPPABLE #-} (Monad m, SubTree1 es m (Layer.Cons layer))
      => Fold.LayerBuilder (Discovery es) m layer where
    layerBuild = Fold.build1 @(Fold.Scoped (Discovery es))
    {-# INLINE layerBuild #-}
