{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Traversal.Deep where

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
import Data.Graph.Traversal.Fold             (Result)
import Data.PtrList.Mutable                  (UnmanagedPtrList)
import Data.Vector.Storable.Foreign          (Vector)
import Foreign.Ptr.Utils                     (SomePtr)
import Type.Data.Bool                        (Not, type (||))


------------------
-- === Deep === --
------------------

-- === Definition === --

data Deep t
type instance Fold.Result     (Deep t) = Fold.Result     t
type instance Fold.LayerScope (Deep t) = Fold.LayerScope t


-- === API === --

type Builder  t = Fold.Builder  (Fold.Scoped (Deep t))
type Builder1 t = Fold.Builder1 (Fold.Scoped (Deep t))

build  :: ∀ t m a.    Builder  t m a => a    -> m (Result t) -> m (Result t)
build1 :: ∀ t m a t1. Builder1 t m a => a t1 -> m (Result t) -> m (Result t)
build  = Fold.build  @(Fold.Scoped (Deep t))
build1 = Fold.build1 @(Fold.Scoped (Deep t))
{-# INLINE build #-}
{-# INLINE build1 #-}

run  :: ∀ t m a.    (Builder  t m a, Mempty (Result t)) => a    -> m (Result t)
run1 :: ∀ t m a t1. (Builder1 t m a, Mempty (Result t)) => a t1 -> m (Result t)
run  = \a -> build  @t a $! pure $! mempty
run1 = \a -> build1 @t a $! pure $! mempty
{-# INLINE run  #-}
{-# INLINE run1 #-}


-- === Instances === --

instance Fold.ComponentBuilder       t  m comp
      => Fold.ComponentBuilder (Deep t) m comp where
    componentBuild = Fold.componentBuild @t
    {-# INLINE componentBuild #-}

instance ( MonadIO m
         , Builder t m Node.Some
         , Layer.Reader Edge.Edge Edge.Source m
         , Layer.Reader Edge.Edge Edge.Target m
         )
      => Fold.LayerBuilder (Deep t) m Type.Type where
    layerBuild = \tpLink acc -> do
        (tp  :: Node.Some) <- Layout.relayout <$> Layer.read @Edge.Source tpLink
        (tgt :: Node.Some) <- Layout.relayout <$> Layer.read @Edge.Target tpLink
        let acc' = Fold.componentBuild @(Deep t) tpLink acc
        if tp == tgt then acc' else Fold.build @(Fold.Scoped (Deep t)) tp acc'
    {-# INLINE layerBuild #-}

instance {-# OVERLAPPABLE #-} (Monad m, Builder1 t m (Layer.Cons layer))
      => Fold.LayerBuilder (Deep t) m layer where
    layerBuild = Fold.build1 @(Fold.Scoped (Deep t))
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

