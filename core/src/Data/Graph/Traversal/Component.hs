-- {-# LANGUAGE Strict               #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Traversal.Component where

import Prologue hiding (Foldable, Foldable1, Traversable, fold, fold1, traverse)

import qualified Control.Monad.State.Layered          as State
import qualified Data.Generics.Traversable            as GTraversable
import qualified Data.Graph.Component.Edge            as Edge
import qualified Data.Graph.Component.Node.Class      as Node
import qualified Data.Graph.Component.Node.Layer.Type as Type
import qualified Data.Graph.Data.Component.Class      as Component
import qualified Data.Graph.Data.Component.Set        as Component
import qualified Data.Graph.Data.Graph.Class          as Graph
import qualified Data.Graph.Data.Layer.Class          as Layer
import qualified Data.Graph.Data.Layer.Layout         as Layout
import qualified Data.Graph.Traversal.Fold            as Fold
import qualified Data.Graph.Traversal.Provider        as Provider
import qualified Data.Map.Strict                      as Map
import qualified Data.PtrSet.Mutable                  as PtrSet
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

import Data.Graph.Traversal.Provider (ComponentDiscovery)


--------------------------------
-- === ComponentDiscovery === --
--------------------------------

-- === Definition === --

-- data ComponentDiscovery comp
-- type instance Fold.Result     (ComponentDiscovery comp) = [Component.Some comp]
-- type instance Fold.LayerScope (ComponentDiscovery comp) = 'Fold.All


-- === API === --

discoverComponents :: ∀ comp a m. Fold.Foldable (ComponentDiscovery comp) m a
    => a -> m [Component.Some comp]
discoverComponents = \a -> Fold.buildFold @(ComponentDiscovery comp) a $! pure $! mempty ; {-# INLINE discoverComponents #-}


-- === Instances === --

instance Monad m => Fold.FoldableComponent (ComponentDiscovery comp) m tag where
    buildComponentFold = \_ a -> a ; {-# INLINE buildComponentFold #-}

instance (MonadIO m, Provider.Provider1 comp m (Layer.Cons layer))
      => Fold.FoldableLayer (ComponentDiscovery comp) m layer where
    buildLayerFold = Provider.gather1 @comp
    {-# INLINE buildLayerFold #-}


-- instance {-# OVERLAPPABLE #-} Monad m
--       => Fold.Foldable1 (ComponentDiscovery comp) m (Component comp') where
--     buildFold1 = \_ a -> a ; {-# INLINE buildFold1 #-}

instance Monad m
      => Fold.Foldable1 (ComponentDiscovery comp) m (Component comp) where
    buildFold1 = \comp mr -> (Layout.relayout comp :) <$> mr
    {-# INLINE buildFold1 #-}

-- instance {-# OVERLAPPABLE #-} Monad m
--       => Fold.Foldable1 (ComponentDiscovery comp) m (Component.Set comp') where
--     buildFold1 = \_ -> id ; {-# INLINE buildFold1 #-}

instance MonadIO m
      => Fold.Foldable1 (ComponentDiscovery comp) m (Component.Set comp) where
    buildFold1 = \a acc -> (\a b -> a <> b) <$> (Layout.relayout <<$>> PtrSet.toList (unwrap a))
                                            <*> acc
    {-# INLINE buildFold1 #-}

-- instance (Monad m, Fold.Foldable (ComponentDiscovery comp) m (PtrSet.UnmanagedPtrSet (Component comp' layout)))
--       => Fold.Foldable1 (ComponentDiscovery comp) m (Component.Set comp') where
--     buildFold1 a = Fold.buildFold @(ComponentDiscovery comp) $! unwrap a
--     {-# INLINE buildFold1 #-}

-- instance {-# OVERLAPPABLE #-} Monad m => Fold.Foldable tag m (PtrSet.UnmanagedPtrSet a) where
--     buildFold = \_ a -> a ; {-# INLINE buildFold #-}

-- instance MonadIO m
--       => Fold.Foldable (ComponentDiscovery comp) m (PtrSet.UnmanagedPtrSet (Component comp layout)) where
--     buildFold = \a acc -> (\a b -> a <> b) <$> (fmap Layout.relayout <$> PtrSet.toList a)
--                                            <*> acc
--     {-# INLINE buildFold #-}

-- instance Monad m => Fold.Foldable (ComponentDiscovery comp) m Bool    where buildFold = \_ a -> a ; {-# INLINE buildFold #-}
-- instance Monad m => Fold.Foldable (ComponentDiscovery comp) m Word8   where buildFold = \_ a -> a ; {-# INLINE buildFold #-}
-- instance Monad m => Fold.Foldable (ComponentDiscovery comp) m Word64  where buildFold = \_ a -> a ; {-# INLINE buildFold #-}
-- instance Monad m => Fold.Foldable (ComponentDiscovery comp) m SomePtr where buildFold = \_ a -> a ; {-# INLINE buildFold #-}




-- instance {-# OVERLAPPABLE #-} Provider tag m (PtrSet.UnmanagedPtrSet a)
-- instance MonadIO m
--       => Provider tag m (PtrSet.UnmanagedPtrSet (Component tag layout)) where
--     gather = \a acc -> (\a b -> a <> b) <$> (Layout.relayout <<$>> PtrSet.toList a)
--                                         <*> acc
--     {-# INLINE gather #-}
