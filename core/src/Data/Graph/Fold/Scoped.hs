{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Fold.Scoped where

import Prologue hiding (Traversable, fold, fold1, traverse)

import qualified Data.Graph.Data.Component.Class  as Component
import qualified Data.Graph.Data.Graph.Class      as Graph
import qualified Data.Graph.Data.Layer.Class      as Layer
import qualified Data.Graph.Fold.Class            as Fold
import qualified Data.Graph.Fold.Struct           as Fold
import qualified Data.Mutable.Class               as Mutable
import qualified Foreign.Ptr                      as Ptr
import qualified Foreign.Storable                 as Storable
import qualified Type.Data.List                   as List

import Data.Graph.Data.Component.Class       (Component)
import Data.Graph.Data.Component.Set         (ComponentSet)
import Data.Graph.Data.Component.Vector      (ComponentVectorA)
import Data.Mutable.Storable.SmallAutoVector (SmallVectorA)
import Data.Vector.Storable.Foreign          (Vector)
import Foreign.Ptr.Utils                     (SomePtr)
import Type.Data.Bool                        (Not)


--------------------
-- === Scoped === --
--------------------

-- === Scope === --

data Scope
    = All
    | Whitelist [Type]
    | Blacklist [Type]

type family LayerScope t :: Scope

type        EnabledLayer   t layer = EnabledLayer__ (LayerScope t) layer
type family EnabledLayer__ t layer where
    EnabledLayer__ 'All             _ = 'True
    EnabledLayer__ ('Whitelist lst) l =      List.In l lst
    EnabledLayer__ ('Blacklist lst) l = Not (List.In l lst)


-- === Definition === --

data Scoped t
type instance Fold.Result (Scoped t) = Fold.Result t
type instance LayerScope  (Scoped t) = LayerScope  t

class Monad m => LayerBuilder t m layer where
    layerBuild :: ∀ layout. Layer.Cons layer layout -> m (Fold.Result t) -> m (Fold.Result t)

class Monad m => ComponentBuilder t m comp where
    componentBuild :: ∀ layout. Component comp layout -> m (Fold.Result t) -> m (Fold.Result t)
    componentBuild = \_ -> id
    {-# INLINE componentBuild #-}


-- === Defaults === --

-- instance {-# OVERLAPPABLE #-} (Monad m, Fold.Builder1 t m (Layer.Cons layer))
--       => LayerBuilder t m layer where
--     layerBuild = Fold.build1 @t
--     {-# INLINE layerBuild #-}


-- === Instances === --

instance {-# OVERLAPPABLE #-}
         ( layers ~ Graph.ComponentLayersM m comp
         , ComponentBuilder t m comp
         , LayersFoldableBuilder__ t layers m )
      => Fold.Builder (Scoped t) m (Component comp layout) where
    build = Fold.build1 @(Scoped t)
    {-# INLINE build #-}

instance {-# OVERLAPPABLE #-}
         ( layers ~ Graph.ComponentLayersM m comp
         , ComponentBuilder t m comp
         , LayersFoldableBuilder__ t layers m )
      => Fold.Builder1 (Scoped t) m (Component comp) where
    build1 = \comp mr -> componentBuild @t comp
        $! buildLayersFold__ @t @layers (Component.unsafeToPtr comp) mr
    {-# INLINE build1 #-}

-- FIXME WD: the below instance is generic. We can use 't' instead of 'Scoped t'
--           but it will overlap then. We need to think for better generalization of it here.
instance {-# OVERLAPPABLE #-}
    (MonadIO m, Fold.Builder1 (Scoped t) m (Component comp))
      => Fold.Builder1 (Scoped t) m (ComponentVectorA alloc comp) where
    build1 = \comp mr -> do
        lst <- Mutable.toList comp
        let f = foldl' (\f a -> f . Fold.build1 @(Scoped t) a) id lst
        f mr
    {-# INLINE build1 #-}

instance {-# OVERLAPPABLE #-}
    (MonadIO m, Fold.Builder1 (Scoped t) m (Component comp))
      => Fold.Builder1 (Scoped t) m (ComponentSet comp) where
    build1 = \comp mr -> do
        lst <- Mutable.toList comp
        let f = foldl' (\f a -> f . Fold.build1 @(Scoped t) a) id lst
        f mr
    {-# INLINE build1 #-}

instance {-# OVERLAPPABLE #-}
    (Monad m, Fold.Builder1 (Fold.Struct (Scoped t)) m a)
      => Fold.Builder1 (Scoped t) m a where
    build1 = Fold.build1 @(Fold.Struct (Scoped t)) ; {-# INLINE build1 #-}

instance Monad m => Fold.Builder (Scoped t) m (Vector a)
instance Monad m => Fold.Builder (Scoped t) m (SmallVectorA s alloc n a)



----------------------
-- === Internal === --
----------------------

-- === FoldableLayers === --

class LayersFoldableBuilder__ t (layers :: [Type]) m where
    buildLayersFold__ :: SomePtr -> m (Fold.Result t) -> m (Fold.Result t)

instance Monad m => LayersFoldableBuilder__ t '[] m where
    buildLayersFold__ = \_ a -> a
    {-# INLINE buildLayersFold__ #-}

instance ( MonadIO m
         , Storable.Storable (Layer.Cons l ())
         , Layer.StorableLayer l m
         , LayerFoldableBuilder__ (EnabledLayer t l) t m l
         , LayersFoldableBuilder__ t ls m )
     => LayersFoldableBuilder__ t (l ': ls) m where
    buildLayersFold__ = \ptr mr -> do
        let fs   = buildLayersFold__ @t @ls ptr'
            ptr' = Ptr.plusPtr ptr $ Layer.byteSize @l
        layerBuild__ @(EnabledLayer t l) @t @m @l ptr $! fs mr
    {-# INLINE buildLayersFold__ #-}


-- === LayerBuilder === --

class Monad m => LayerFoldableBuilder__ (active :: Bool) t m layer where
    layerBuild__ :: SomePtr -> m (Fold.Result t) -> m (Fold.Result t)

instance {-# OVERLAPPABLE #-} Monad m
      => LayerFoldableBuilder__ 'False t m layer where
    layerBuild__ = \_ a -> a
    {-# INLINE layerBuild__ #-}

instance (MonadIO m, Layer.StorableLayer layer m, LayerBuilder t m layer)
      => LayerFoldableBuilder__ 'True t m layer where
    layerBuild__ = \ptr mr -> do
        layer <- Layer.unsafePeekWrapped @layer ptr
        -- r     <- mr -- | We've observed better performance here, but it
        --                  makes the evaluation reversed, which is bad for cache
        -- putStrLn $ "$ layerBuild " <> show ptr
        layerBuild @t @m @layer layer mr
    {-# INLINE layerBuild__ #-}
