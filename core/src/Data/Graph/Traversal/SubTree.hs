-- {-# LANGUAGE Strict               #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Traversal.SubTree where

import Prologue hiding (Foldable, Foldable1, Traversable, fold, fold1, traverse)

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
import Data.Graph.Component.Node.Layer.Model (Model)
import Data.Graph.Data.Component.Class       (Component)
import Data.PtrList.Mutable                  (UnmanagedPtrList)
import Data.Set                              (Set)
import Data.Vector.Storable.Foreign          (Vector)
import Foreign.Ptr.Utils                     (SomePtr)
import Type.Data.Bool                        (Not, type (||))


------------------------------
-- === SubTreeDiscovery === --
------------------------------

-- === Definition === --

data SubTreeDiscovery
type instance Fold.Result     SubTreeDiscovery = [Component.Any]
type instance Fold.LayerScope SubTreeDiscovery = 'Fold.Whitelist '[Model, Type.Type]


-- === API === --

getSubTree :: Fold.Foldable (Fold.DepthFold SubTreeDiscovery) m a => a -> m [Component.Any]
getSubTree = \a -> Fold.buildFold @(Fold.DepthFold SubTreeDiscovery) a $! pure mempty ; {-# INLINE getSubTree #-}


-- === Instances === --

instance Monad m
      => Fold.FoldableComponent SubTreeDiscovery m tag where
    buildComponentFold = \comp acc -> do
        a <- acc
        pure $! Layout.relayout comp : a
    {-# INLINE buildComponentFold #-}

instance ( MonadIO m
         , Fold.Foldable (Fold.DepthFold SubTreeDiscovery) m Node.Some
         , Layer.Reader Edge.Edge Edge.Source m
         , Layer.Reader Edge.Edge Edge.Target m
         )
      => Fold.FoldableLayer SubTreeDiscovery m Type.Type where
    buildLayerFold = \tpLink acc -> do
        (tp  :: Node.Some) <- Layout.relayout <$> Layer.read @Edge.Source tpLink
        (tgt :: Node.Some) <- Layout.relayout <$> Layer.read @Edge.Target tpLink
        a <- acc
        let acc' = pure $! Layout.relayout tpLink : a
        if tp == tgt then acc' else Fold.buildFold @(Fold.DepthFold SubTreeDiscovery) tp acc'
    {-# INLINE buildLayerFold #-}


instance {-# OVERLAPPABLE #-} (Monad m, Fold.Foldable1 (Fold.DepthFold SubTreeDiscovery) m (Layer.Cons layer))
      => Fold.FoldableLayer SubTreeDiscovery m layer where
    buildLayerFold = Fold.buildFold1 @(Fold.DepthFold SubTreeDiscovery)
    {-# INLINE buildLayerFold #-}
