-- {-# LANGUAGE Strict               #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Traversal.Discovery where

import Prologue hiding (Foldable, Foldable1, Traversable, fold, fold1, traverse)

import qualified Control.Monad.State.Layered          as State
import qualified Data.Generics.Traversable            as GTraversable
import qualified Data.Graph.Class                     as Graph
import qualified Data.Graph.Component.Edge            as Edge
import qualified Data.Graph.Component.Node.Class      as Node
import qualified Data.Graph.Component.Node.Layer.Type as Type
import qualified Data.Graph.Data.Component.Class      as Component
import qualified Data.Graph.Data.Component.Dynamic    as Component
import qualified Data.Graph.Data.Component.Dynamic    as Dynamic
import qualified Data.Graph.Data.Container.Set        as Setx
import qualified Data.Graph.Data.Layer.Class          as Layer
import qualified Data.Graph.Data.Layer.Layout         as Layout
import qualified Data.Map.Strict                      as Map
import qualified Data.Set                             as Set
import qualified Foreign.Ptr                          as Ptr
import qualified Foreign.Storable                     as Storable

import Data.Graph.Data.Component.Class    (Component)
import Data.Graph.Data.Component.Provider (DynamicTraversalMap (..))
import Data.Set                           (Set)

import Data.Generics.Traversable (GTraversable)
import Foreign.Ptr.Utils         (SomePtr)

import Data.Graph.Component.Node.Layer.Model (Model)
import Data.PtrList.Mutable                  (UnmanagedPtrList)
import Data.Vector.Storable.Foreign          (Vector)



---------------------------------
-- === Component Discovery === --
---------------------------------

-- === API === --

discover :: (MonadIO m, State.Getter DynamicTraversalMap m, Typeable tag)
    => Component tag layout -> m (Set Component.Dynamic)
discover = discoverDynamic . Component.toDynamic1
{-# INLINE discover #-}

discoverDynamic :: (MonadIO m, State.Getter DynamicTraversalMap m)
    => Component.Dynamic -> m (Set Component.Dynamic)
discoverDynamic comp = do
    info <- State.get @DynamicTraversalMap
    discoverDynamic__ info mempty comp
{-# INLINE discoverDynamic #-}

discoverDynamic__ :: MonadIO m
    => DynamicTraversalMap -> Set Component.Dynamic -> Component.Dynamic
    -> m (Set Component.Dynamic)
discoverDynamic__ info = go where
    go all comp = do
        nbrs <- getNeighboursx info comp
        let newComps = filter (flip Set.notMember all) nbrs
            all'     = foldr Set.insert all newComps
        foldM go all' newComps
{-# INLINE discoverDynamic__ #-}

getNeighboursx :: MonadIO m
    => DynamicTraversalMap -> Component.Dynamic -> m [Component.Dynamic]
getNeighboursx info comp = neighbours where
    Component.Rep tagRep _ = comp ^. Dynamic.rep
    compPtr    = comp ^. Dynamic.ptr
    compInfo   = unsafeFromJust $ Map.lookup tagRep $ unwrap info  -- TODO: convert to panic
    neighbours = liftIO $ compInfo compPtr
{-# INLINE getNeighboursx #-}






----------------------
-- === Foldable === --
----------------------

-- === Definition === --

type family Result t
type family EnabledLayer t layer :: Bool

class Monad m => Foldable t m a where
    fold :: a -> (Result t) -> m (Result t)

class Monad m => Foldable1 t m a where
    fold1 :: ∀ t1. a t1 -> (Result t) -> m (Result t)

class Monad m => FoldableLayer t m layer where
    foldLayer :: ∀ layout. Layer.Cons layer layout -> Result t -> m (Result t)

class Monad m => FoldableComponent t m tag where
    foldComponent :: ∀ layout. Component tag layout -> Result t -> m (Result t)


-- === Generics === --

gfold :: ∀ t a m. (GTraversable (Foldable t m) a, Applicative m)
      => a -> (Result t) -> m (Result t)
gfold = GTraversable.gfoldl' @(Foldable t m) (\r d x -> r =<< fold @t d x) pure
{-# INLINE gfold #-}

instance {-# OVERLAPPABLE #-} (GTraversable (Foldable t m) a, Monad m)
      => Foldable t m a where
    fold = gfold @t ; {-# INLINE fold #-}


-- === Instances === --

instance ( layers ~ Graph.DiscoverComponentLayers m tag
         , Monad m
         , FoldableComponent t m tag
         , LayersFoldableBuilder__ t layers m )
      => Foldable t m (Component tag layout) where
    fold comp mr = do
        r <- buildLayersFold__ @t @layers (Component.unsafeToPtr comp) (pure mr)
        foldComponent @t comp r
    {-# INLINE fold #-}



----------------------
-- === Internal === --
----------------------

-- === FoldableLayers === --

class LayersFoldableBuilder__ t (layers :: [Type]) m where
    buildLayersFold__ :: SomePtr -> m (Result t) -> m (Result t)

instance Monad m => LayersFoldableBuilder__ t '[] m where
    buildLayersFold__ _ = id ; {-# INLINE buildLayersFold__ #-}

instance ( MonadIO m
         , Storable.Storable (Layer.Cons l ())
         , Layer.StorableLayer l m
         , LayerFoldableBuilder__ (EnabledLayer t l) t m l
         , LayersFoldableBuilder__ t ls m )
     => LayersFoldableBuilder__ t (l ': ls) m where
    buildLayersFold__ ptr mr = do
        let f    = buildLayerFold__ @(EnabledLayer t l) @t @m @l ptr
            fs   = buildLayersFold__ @t @ls ptr'
            ptr' = Ptr.plusPtr ptr $ Layer.byteSize @l
            out  = f $! fs mr
        out
    {-# INLINE buildLayersFold__ #-}


-- === FoldableLayer === --

class Monad m => LayerFoldableBuilder__ (active :: Bool) t m layer where
    buildLayerFold__ :: SomePtr -> m (Result t) -> m (Result t)

instance {-# OVERLAPPABLE #-} Monad m
      => LayerFoldableBuilder__ 'False t m layer where
    buildLayerFold__ _ = id ; {-# INLINE buildLayerFold__ #-}


instance (Monad m, Layer.StorableLayer layer m, FoldableLayer t m layer)
      => LayerFoldableBuilder__ 'True t m layer where
    buildLayerFold__ ptr mr = do
        layer <- Layer.unsafePeekWrapped @layer ptr
        r     <- mr
        foldLayer @t @m @layer layer r
    {-# INLINE buildLayerFold__ #-}








-----------------------
-- === Discovery === --
-----------------------

-- === Definition === --

data Discovery
type instance Result Discovery = [Component.Any]
type instance EnabledLayer Discovery layer = DiscoveryEnabledLayer layer
type family DiscoveryEnabledLayer layer where
    DiscoveryEnabledLayer Model     = 'True
    DiscoveryEnabledLayer Type.Type = 'True
    DiscoveryEnabledLayer _         = 'False


-- === API === --

getNeighbours :: Foldable Discovery m a => a -> m [Component.Any]
getNeighbours a = fold @Discovery a mempty ; {-# INLINE getNeighbours #-}


-- === Instances === --

instance Monad m => FoldableComponent Discovery m tag where
    foldComponent comp = pure . (Layout.relayout comp :) ; {-# INLINE foldComponent #-}

instance (Foldable1 Discovery m (Layer.Cons Model), Monad m)
      => FoldableLayer Discovery m Model where
    foldLayer layer acc = fold1 @Discovery layer acc ; {-# INLINE foldLayer #-}

instance ( MonadIO m
         , Foldable1 Discovery m (Layer.Cons Model)
         , Foldable  Discovery m (Node.Node ())
         , Layer.Reader Edge.Edge Edge.Source m
         , Layer.Reader Edge.Edge Edge.Target m
         )
      => FoldableLayer Discovery m Type.Type where
    foldLayer layer acc = do
        (src :: Node.Node ()) <- Layout.relayout <$> Layer.read @Edge.Source layer
        (tgt :: Node.Node ()) <- Layout.relayout <$> Layer.read @Edge.Target layer
        let f     = if src == tgt then pure else fold @Discovery src
            acc'  = Layout.relayout layer : acc
            acc'' = f acc'
        acc''
    {-# INLINE foldLayer #-}








instance GTraversable ctx (UnmanagedPtrList a) where gtraverse _ = error "e1"
instance GTraversable ctx (Vector a) where gtraverse _ = error "e2"
instance GTraversable ctx (Setx.Set a k) where gtraverse _ = error "e3"
