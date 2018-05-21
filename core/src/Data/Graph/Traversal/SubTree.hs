-- {-# LANGUAGE Strict               #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Traversal.SubTree where

import Prologue hiding (Foldable, Foldable1, Traversable, Traversal, Traversal',
                 fold, fold1, traverse)

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
import qualified Data.Graph.Traversal.Provider        as Provider
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

type Traversal  es = Fold.Foldable  (Fold.DepthFold (Discovery es))
type Traversal1 es = Fold.Foldable1 (Fold.DepthFold (Discovery es))

discover :: ∀ es m a. Traversal es m a => a -> m [Component.Any]
discover = \a -> Fold.buildFold @(Fold.DepthFold (Discovery es)) a $! pure mempty
{-# INLINE discover #-}


-- === Simple === --

type SimpleDiscoveryTarget = '[Model, Type.Type, Source]
type Discovery'  = Discovery SimpleDiscoveryTarget
type Traversal'  = Fold.Foldable  (Fold.DepthFold Discovery')
type Traversal1' = Fold.Foldable1 (Fold.DepthFold Discovery')

discoverSimple :: ∀ m a. Traversal' m a => a -> m [Component.Any]
discoverSimple = discover @SimpleDiscoveryTarget
{-# INLINE discoverSimple #-}


-- === Instances === --

instance Monad m
      => Fold.FoldableComponent (Discovery es) m tag where
    buildComponentFold = \comp acc -> do
        a <- acc
        pure $! Layout.relayout comp : a
    {-# INLINE buildComponentFold #-}

instance ( MonadIO m
         , Traversal es m Node.Some
         , Layer.Reader Edge.Edge Edge.Source m
         , Layer.Reader Edge.Edge Edge.Target m
         )
      => Fold.FoldableLayer (Discovery es) m Type.Type where
    buildLayerFold = \tpLink acc -> do
        (tp  :: Node.Some) <- Layout.relayout <$> Layer.read @Edge.Source tpLink
        (tgt :: Node.Some) <- Layout.relayout <$> Layer.read @Edge.Target tpLink
        a <- acc
        let acc' = pure $! Layout.relayout tpLink : a
        if tp == tgt then acc'
                     else Fold.buildFold @(Fold.DepthFold (Discovery es)) tp acc'
    {-# INLINE buildLayerFold #-}


instance {-# OVERLAPPABLE #-} (Monad m, Traversal1 es m (Layer.Cons layer))
      => Fold.FoldableLayer (Discovery es) m layer where
    buildLayerFold = Fold.buildFold1 @(Fold.DepthFold (Discovery es))
    {-# INLINE buildLayerFold #-}
