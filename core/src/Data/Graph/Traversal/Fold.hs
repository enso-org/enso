-- {-# LANGUAGE Strict               #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Traversal.Fold where

import Prologue hiding (Foldable, Foldable1, Traversable, fold, fold1, traverse)

import qualified Control.Monad.State.Layered     as State
import qualified Data.Generics.Traversable       as GTraversable
import qualified Data.Graph.Data.Component.Class as Component
import qualified Data.Graph.Data.Component.Set   as Setx
import qualified Data.Graph.Data.Graph.Class     as Graph
import qualified Data.Graph.Data.Layer.Class     as Layer
import qualified Data.Graph.Data.Layer.Layout    as Layout
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import qualified Foreign.Ptr                     as Ptr
import qualified Foreign.Storable                as Storable
import qualified Type.Data.List                  as List

import Data.Generics.Traversable       (GTraversable)
import Data.Graph.Data.Component.Class (Component)
import Data.PtrList.Mutable            (UnmanagedPtrList)
import Data.Set                        (Set)
import Data.Vector.Storable.Foreign    (Vector)
import Foreign.Ptr.Utils               (SomePtr)
import Type.Data.Bool                  (Not, type (||))


----------------------
-- === Foldable === --
----------------------

-- === Definition === --

data Scope
    = All
    | Whitelist [Type]
    | Blacklist [Type]

type family Result     t
type family LayerScope t :: Scope

type        EnabledLayer   t layer = EnabledLayer__ (LayerScope t) layer
type family EnabledLayer__ t layer where
    EnabledLayer__ 'All             _ = 'True
    EnabledLayer__ ('Whitelist lst) l =      List.In l lst
    EnabledLayer__ ('Blacklist lst) l = Not (List.In l lst)

class Monad m => Foldable t m a where
    buildFold :: a -> m (Result t) -> m (Result t)

class Monad m => Foldable1 t m a where
    buildFold1 :: ∀ t1. a t1 -> m (Result t) -> m (Result t)

class Monad m => FoldableLayer t m layer where
    buildLayerFold :: ∀ layout. Layer.Cons layer layout -> m (Result t) -> m (Result t)

class Monad m => FoldableComponent t m tag where
    buildComponentFold :: ∀ layout. Component tag layout -> m (Result t) -> m (Result t)


-- === Generics === --

gbuildFold :: ∀ t a m. (GTraversable (Foldable t m) a, Applicative m)
      => a -> m (Result t) -> m (Result t)
gbuildFold = GTraversable.gfoldl' @(Foldable t m) (\r d x -> r $! buildFold @t d x) id
{-# INLINE gbuildFold #-}

instance {-# OVERLAPPABLE #-} (GTraversable (Foldable t m) a, Monad m)
      => Foldable t m a where
    buildFold = gbuildFold @t ; {-# INLINE buildFold #-}

instance {-# OVERLAPPABLE #-} (Foldable1 s m t, Monad m)
      => Foldable s m (t a) where
    buildFold = buildFold1 @s ; {-# INLINE buildFold #-}


-- === Instances === --

instance {-# OVERLAPPABLE #-} ( layers ~ Graph.DiscoverComponentLayers m tag
         , Monad m
         , FoldableComponent t m tag
         , LayersFoldableBuilder__ t layers m )
      => Foldable t m (Component tag layout) where
    buildFold = \comp mr -> buildComponentFold @t comp
        $! buildLayersFold__ @t @layers (Component.unsafeToPtr comp) mr
    {-# INLINE buildFold #-}

instance {-# OVERLAPPABLE #-} (Monad m, Foldable1 t m (Layer.Cons layer))
      => FoldableLayer t m layer where
    buildLayerFold = buildFold1 @t ; {-# INLINE buildLayerFold #-}



----------------------
-- === Internal === --
----------------------

-- === FoldableLayers === --

class LayersFoldableBuilder__ t (layers :: [Type]) m where
    buildLayersFold__ :: SomePtr -> m (Result t) -> m (Result t)

instance Monad m => LayersFoldableBuilder__ t '[] m where
    buildLayersFold__ = \_ a -> a ; {-# INLINE buildLayersFold__ #-}

instance ( MonadIO m
         , Storable.Storable (Layer.Cons l ())
         , Layer.StorableLayer l m
         , LayerFoldableBuilder__ (EnabledLayer t l) t m l
         , LayersFoldableBuilder__ t ls m )
     => LayersFoldableBuilder__ t (l ': ls) m where
    buildLayersFold__ = \ptr mr -> do
        let fs   = buildLayersFold__ @t @ls ptr'
            ptr' = Ptr.plusPtr ptr $ Layer.byteSize @l
        buildLayerFold__ @(EnabledLayer t l) @t @m @l ptr $! fs mr
    {-# INLINE buildLayersFold__ #-}


-- === FoldableLayer === --

class Monad m => LayerFoldableBuilder__ (active :: Bool) t m layer where
    buildLayerFold__ :: SomePtr -> m (Result t) -> m (Result t)

instance {-# OVERLAPPABLE #-} Monad m
      => LayerFoldableBuilder__ 'False t m layer where
    buildLayerFold__ = \_ a -> a ; {-# INLINE buildLayerFold__ #-}

instance (Monad m, Layer.StorableLayer layer m, FoldableLayer t m layer)
      => LayerFoldableBuilder__ 'True t m layer where
    buildLayerFold__ = \ptr mr -> do
        layer <- Layer.unsafePeekWrapped @layer ptr
        r     <- mr -- | Performance
        buildLayerFold @t @m @layer layer (pure r)
    {-# INLINE buildLayerFold__ #-}





-- FIXME !!!

instance GTraversable ctx (UnmanagedPtrList a) where gtraverse _ = error "e1"
instance GTraversable ctx (Vector a) where gtraverse _ = error "e2"
instance GTraversable ctx (Setx.Set a k) where gtraverse _ = error "e3"
