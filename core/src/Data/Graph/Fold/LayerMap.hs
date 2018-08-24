{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Fold.LayerMap where

import Prologue hiding (Traversable, fold, fold1, traverse)

import qualified Control.Monad.State.Layered      as State
import qualified Data.Generics.Traversable        as GTraversable
import qualified Data.Graph.Data.Component.Class  as Component
import qualified Data.Graph.Data.Component.Set    as ComponentSet
import qualified Data.Graph.Data.Component.Vector as ComponentVector
import qualified Data.Graph.Data.Graph.Class      as Graph
import qualified Data.Graph.Data.Layer.Class      as Layer
import qualified Data.Graph.Data.Layer.Layout     as Layout
import qualified Data.Graph.Fold.Class            as Fold
import qualified Data.Graph.Fold.Struct           as Fold
import qualified Data.Map.Strict                  as Map
import qualified Data.Mutable.Class               as Mutable
import qualified Data.Set                         as Set
import qualified Foreign.Ptr                      as Ptr
import qualified Foreign.Storable                 as Storable
import qualified Type.Data.List                   as List

import Data.Generics.Traversable             (GTraversable)
import Data.Graph.Data.Component.Class       (Component)
import Data.Graph.Data.Component.Set         (ComponentSetA)
import Data.Graph.Data.Component.Vector      (ComponentVectorA)
import Data.Mutable.Storable.SmallAutoVector (SmallVectorA)
import Data.Set                              (Set)
import Data.Vector.Storable.Foreign          (Vector)
import Foreign.Ptr                           (Ptr)
import Foreign.Ptr.Utils                     (SomePtr)
import Type.Data.Bool                        (Not, type (||))

import qualified Type.Show as Type

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

class Monad m => LayerMap t m layer where
    mapLayerPtr :: ∀ layout. Ptr (Layer.Cons layer layout)
                -> m (Fold.Result t) -> m (Fold.Result t)


-- === Instances === --

instance {-# OVERLAPPABLE #-}
    ( layers ~ Graph.ComponentLayersM m comp
    , LayersFoldableBuilder__ t layers m
    , Monad m
    ) => Fold.Builder (Scoped t) m (Component comp layout) where
    build = Fold.build1 @(Scoped t)
    {-# INLINE build #-}

instance {-# OVERLAPPABLE #-}
    ( layers ~ Graph.ComponentLayersM m comp
    , LayersFoldableBuilder__ t layers m
    , Monad m
    ) => Fold.Builder1 (Scoped t) m (Component comp) where
    build1 = buildLayersFold__ @t @layers . Component.unsafeToPtr
    {-# INLINE build1 #-}

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
      => Fold.Builder1 (Scoped t) m (ComponentSetA alloc comp) where
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
instance Monad m => Fold.Builder (Scoped s) m (SmallVectorA t alloc n a)



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


-- === LayerMap === --

class Monad m => LayerFoldableBuilder__ (active :: Bool) t m layer where
    layerBuild__ :: SomePtr -> m (Fold.Result t) -> m (Fold.Result t)

instance {-# OVERLAPPABLE #-} Monad m
      => LayerFoldableBuilder__ 'False t m layer where
    layerBuild__ = \_ a -> a
    {-# INLINE layerBuild__ #-}

instance (Monad m, Layer.StorableLayer layer m, LayerMap t m layer)
      => LayerFoldableBuilder__ 'True t m layer where
    layerBuild__ = \ptr mr -> do
        r <- mr -- | Performance
        mapLayerPtr @t @m @layer (coerce ptr) (pure r)
    {-# INLINE layerBuild__ #-}
