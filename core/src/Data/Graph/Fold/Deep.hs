{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Fold.Deep where

import Prologue hiding (Traversable, Traversal, Traversal', fold, fold1,
                 traverse)

import qualified Data.Graph.Component.Edge            as Edge
import qualified Data.Graph.Component.Node.Class      as Node
import qualified Data.Graph.Component.Node.Layer.Type as Type
import qualified Data.Graph.Data.Layer.Class          as Layer
import qualified Data.Graph.Data.Layer.Layout         as Layout
import qualified Data.Graph.Fold.Class                as Fold
import qualified Data.Graph.Fold.Scoped               as Fold

import Data.Graph.Fold.Class (Result)



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

build  :: ∀ t m a.    Builder  t m a => a    -> Fold.Transformation m t
build1 :: ∀ t m a t1. Builder1 t m a => a t1 -> Fold.Transformation m t
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
         , Fold.ComponentBuilder t m Edge.Edges
         )
      => Fold.LayerBuilder (Deep t) m Type.Type where
    layerBuild = \tpLink acc -> do
        (tp  :: Node.Some) <- Layout.relayout <$> Layer.read @Edge.Source tpLink
        (tgt :: Node.Some) <- Layout.relayout <$> Layer.read @Edge.Target tpLink
        acc' <- Fold.componentBuild @(Deep t) tpLink acc
        if tp == tgt then pure acc'
                     else Fold.build @(Fold.Scoped (Deep t)) tp (pure acc')
    {-# INLINE layerBuild #-}

instance {-# OVERLAPPABLE #-} (Monad m, Builder1 t m (Layer.Cons layer))
      => Fold.LayerBuilder (Deep t) m layer where
    layerBuild = Fold.build1 @(Fold.Scoped (Deep t))
    {-# INLINE layerBuild #-}

