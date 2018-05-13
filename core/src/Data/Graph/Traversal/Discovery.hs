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
discoverDynamic !comp = do
    !info <- State.get @DynamicTraversalMap
    discoverDynamic__ info mempty comp
{-# INLINE discoverDynamic #-}

discoverDynamic__ :: MonadIO m
    => DynamicTraversalMap -> Set Component.Dynamic -> Component.Dynamic
    -> m (Set Component.Dynamic)
discoverDynamic__ !info = go where
    go !all !comp = do
        !nbrs <- getNeighboursx info comp
        let !newComps = filter (flip Set.notMember all) nbrs
            !all'     = foldr Set.insert all newComps
        foldM go all' newComps
{-# INLINE discoverDynamic__ #-}

getNeighboursx :: MonadIO m
    => DynamicTraversalMap -> Component.Dynamic -> m [Component.Dynamic]
getNeighboursx info comp = neighbours where
    Component.Rep tagRep !_ = comp ^. Dynamic.rep
    !compPtr    = comp ^. Dynamic.ptr
    !compInfo   = unsafeFromJust $ Map.lookup tagRep $ unwrap info  -- TODO: convert to panic
    !neighbours = liftIO $ compInfo compPtr
{-# INLINE getNeighboursx #-}



data Discovery

type family Result t
type family TraversableLayer t layer :: Bool

type instance Result Discovery = [Component.Any]

type instance TraversableLayer Discovery layer = DiscoveryLayers layer
type family DiscoveryLayers layer where
    DiscoveryLayers Model     = 'True
    DiscoveryLayers Type.Type = 'True
    DiscoveryLayers _         = 'False





getNeighbours :: Foldable Discovery m a => a -> (m [Component.Any] -> m [Component.Any])
getNeighbours = fold @Discovery

class Monad m => Foldable t m a where
    fold :: a -> (m (Result t) -> m (Result t))

class Monad m => Foldable1 t m a where
    fold1 :: ∀ t1. a t1 -> (m (Result t) -> m (Result t))

instance {-# OVERLAPPABLE #-} (GTraversable (Foldable t m) a, Monad m)
      => Foldable t m a where
    fold = gfold @t ; {-# INLINE fold #-}

gfold :: ∀ t a m. (GTraversable (Foldable t m) a)
      => a -> (m (Result t) -> m (Result t))
gfold = GTraversable.gfoldl' @(Foldable t m) (\r d x -> r $! fold @t d x) id ; {-# INLINE gfold #-}

instance (Monad m, FoldableLayers t (Graph.DiscoverComponentLayers m tag) m, t ~ Discovery) -- FIXME
      => Foldable t m (Component tag layout) where
    fold !t = fmap (fmap (Layout.relayout t :))
        $! foldLayers @t @(Graph.DiscoverComponentLayers m tag) $! (Component.unsafeToPtr t)
    {-# INLINE fold #-}






class FoldableLayers t (layers :: [Type]) m where
    foldLayers :: SomePtr -> (m (Result t) -> m (Result t))

instance Monad m => FoldableLayers t '[] m where
    foldLayers _ = id ; {-# INLINE foldLayers #-}

instance (MonadIO m, Storable.Storable (Layer.Cons l ()), Layer.StorableLayer l m
         , FoldableLayer__ (TraversableLayer Discovery l) Discovery m l, FoldableLayers Discovery ls m)
     => FoldableLayers Discovery (l ': ls) m where
    foldLayers ptr mr = do
        let f    = foldLayer__ @(TraversableLayer Discovery l) @Discovery @m @l ptr
            fs   = foldLayers @Discovery @ls ptr'
            ptr' = Ptr.plusPtr ptr $ Layer.byteSize @l
            out  = f $! fs mr
        out
    {-# INLINE foldLayers #-}


class Monad m => FoldableLayer t m layer where
    foldLayer :: ∀ layout. Layer.Cons layer layout -> Result t -> m (Result t)

class Monad m => FoldableLayer__ (active :: Bool) t m layer where
    foldLayer__ :: SomePtr -> (m (Result t) -> m (Result t))

instance {-# OVERLAPPABLE #-} Monad m
      => FoldableLayer__ 'False t m layer where
    foldLayer__ _ = id ; {-# INLINE foldLayer__ #-}


instance (Monad m, Layer.StorableLayer layer m, FoldableLayer t m layer)
      => FoldableLayer__ 'True t m layer where
    foldLayer__ !ptr !mr = do
        layer <- Layer.unsafePeekWrapped @layer ptr
        r     <- mr
        foldLayer @t @m @layer layer r
    {-# INLINE foldLayer__ #-}


instance (Foldable1 t m (Layer.Cons Model), Monad m)
      => FoldableLayer t m Model where
    foldLayer layer acc = fold1 @t layer (pure acc) ; {-# INLINE foldLayer #-}


instance ( MonadIO m
         , Foldable1 t m (Layer.Cons Model)
         , Foldable  t m (Node.Node ())
         , Layer.Reader Edge.Edge Edge.Source m
         , Layer.Reader Edge.Edge Edge.Target m
         , t ~ Discovery -- FIXME
         )
      => FoldableLayer t m Type.Type where
    foldLayer layer acc = do
        (src :: Node.Node ()) <- Layout.relayout <$> Layer.read @Edge.Source layer
        (tgt :: Node.Node ()) <- Layout.relayout <$> Layer.read @Edge.Target layer
        let f     = if src == tgt then id else fold @t src
            acc'  = Layout.relayout layer : acc
            acc'' = f (pure acc')
        acc''
    {-# INLINE foldLayer #-}



instance GTraversable ctx (UnmanagedPtrList a) where gtraverse _ = error "e1"
instance GTraversable ctx (Vector a) where gtraverse _ = error "e2"
instance GTraversable ctx (Setx.Set a k) where gtraverse _ = error "e3"
