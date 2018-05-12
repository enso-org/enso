-- {-# LANGUAGE Strict               #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Traversal.Discovery where

import Prologue

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



class Monad m => GetNeighbours m t where
    getNeighbours :: t -> (m [SomePtr] -> m [SomePtr])

class Monad m => GetNeighbours1 m t where
    getNeighbours1 :: ∀ a. t a -> (m [SomePtr] -> m [SomePtr])

instance {-# OVERLAPPABLE #-} (GTraversable (GetNeighbours m) t, Monad m)
      => GetNeighbours m t where
    getNeighbours = ggetNeighbours ; {-# INLINE getNeighbours #-}

ggetNeighbours :: ∀ t m. (GTraversable (GetNeighbours m) t)
               => t -> (m [SomePtr] -> m [SomePtr])
ggetNeighbours = GTraversable.gfoldl' @(GetNeighbours m)
                 (\r d x -> r $! getNeighbours d x) id
{-# INLINE ggetNeighbours #-}

instance (Monad m, GetLayersNeighbours m (Graph.DiscoverComponentLayers m tag))
      => GetNeighbours m (Component tag layout) where
    getNeighbours !t = fmap (fmap (Component.unsafeToPtr t :))
        $! getLayersNeighbours @m @(Graph.DiscoverComponentLayers m tag) $! (Component.unsafeToPtr t)
    {-# INLINE getNeighbours #-}


class GetLayersNeighbours m (layers :: [Type]) where
    getLayersNeighbours :: SomePtr -> (m [SomePtr] -> m [SomePtr])

instance Monad m => GetLayersNeighbours m '[] where
    getLayersNeighbours !_ = id ; {-# INLINE getLayersNeighbours #-}

instance (MonadIO m, Storable.Storable (Layer.Cons l ()), Layer.StorableLayer l m
         , GetLayerNeighbours m l, GetLayersNeighbours m ls)
     => GetLayersNeighbours m (l ': ls) where
    getLayersNeighbours !ptr !mr = do
        -- !l <- Layer.unsafePeekWrapped @l ptr
        -- mr
        let !f    = getLayerNeighbours  @m @l  ptr
            !fs   = getLayersNeighbours @m @ls ptr'
            !ptr' = Ptr.plusPtr ptr $ Layer.byteSize @l
            !out  = f $! fs mr
        out
        -- f $! fs mr
        -- undefined
    {-# INLINE getLayersNeighbours #-}
        -- l <- liftIO $ Storable.peek (coerce ptr) :: m (Layer.Cons l ())
        -- undefined

class Monad m => GetLayerNeighbours m layer where
    getLayerNeighbours :: ∀ layout. SomePtr -> (m [SomePtr] -> m [SomePtr])


instance {-# OVERLAPPABLE #-} Monad m
      => GetLayerNeighbours m layer where
    getLayerNeighbours !_ !a = a ; {-# INLINE getLayerNeighbours #-}


instance (GetNeighbours1 m (Layer.Cons Model), Monad m, Layer.StorableLayer Model m)
      => GetLayerNeighbours m Model where
    getLayerNeighbours !ptr !mr = do
        !layer <- Layer.unsafePeekWrapped @Model ptr
        let !f   = getNeighbours1 layer
            !out = f mr
        out
    {-# INLINE getLayerNeighbours #-}



-- instance (GetNeighbours1 m (Layer.Cons Model), Monad m)
--       => GetLayerNeighbours m Model where
--     getLayerNeighbours = getNeighbours1 ; {-# INLINE getLayerNeighbours #-}


instance ( MonadIO m
         , GetNeighbours1 m (Layer.Cons Model)
         , GetNeighbours  m (Node.Node ())
         , Layer.Reader Edge.Edge Edge.Source m
         , Layer.Reader Edge.Edge Edge.Target m
         )
      => GetLayerNeighbours m Type.Type where
    getLayerNeighbours !ptr !mr = do
        !layer <- Layer.unsafePeekWrapped @Type.Type ptr
        !(src :: Node.Node ()) <- Layout.relayout <$> Layer.read @Edge.Source layer
        !(tgt :: Node.Node ()) <- Layout.relayout <$> Layer.read @Edge.Target layer
        let f = if src == tgt then id else getNeighbours src ; {-# INLINE f #-}
            !mr'  = (Component.unsafeToPtr layer :) <$> mr
            !mr'' = f mr'
        mr''
    {-# INLINE getLayerNeighbours #-}

instance GTraversable ctx (UnmanagedPtrList a) where gtraverse _ = error "e1"
instance GTraversable ctx (Vector a) where gtraverse _ = error "e2"
instance GTraversable ctx (Setx.Set a k) where gtraverse _ = error "e3"
