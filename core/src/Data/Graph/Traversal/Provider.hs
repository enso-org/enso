{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Traversal.Provider where

import Prologue

import qualified Data.Generics.Traversable       as GTraversable
import qualified Data.Graph.Data.Component.Class as Component
import qualified Data.Graph.Data.Layer.Class     as Layer
import qualified Data.Graph.Data.Layer.Layout    as Layout
import qualified Data.Graph.Traversal.Fold       as Fold
import qualified Data.PtrList.Mutable            as PtrList
import qualified Data.PtrSet.Mutable             as PtrSet
import qualified Data.Vector.Storable.Foreign    as Foreign

import Data.Generics.Traversable       (GTraversable)
import Data.Graph.Data.Component.Class (Component)
import Data.Graph.Data.Component.Set   (Set (Set))
import Data.Map.Strict                 (Map)
import Foreign.Ptr.Utils               (SomePtr)


-- data ComponentDiscovery comp
-- type instance Fold.Result     (ComponentDiscovery comp) = [Component.Some comp]
-- type instance Fold.LayerScope (ComponentDiscovery comp) = 'Fold.All


-- ----------------------
-- -- === Provider === --
-- ----------------------

-- -- === Definition === --

-- class Provider tag m a where
--     gather :: a -> m [Component.Some tag] -> m [Component.Some tag]
--     gather = \_ a -> a ; {-# INLINE gather #-}

-- class Provider1 tag m a where
--     gather1 :: ∀ t. a t -> m [Component.Some tag] -> m [Component.Some tag]
--     gather1 = \_ a -> a ; {-# INLINE gather1 #-}

-- ggather :: ∀ tag m a. (GTraversable (Provider tag m) a)
--         => a -> m [Component.Some tag] -> m [Component.Some tag]
-- ggather = GTraversable.gfoldl' @(Provider tag m) (\f a x -> f $! gather @tag a x) (\a -> a)
-- {-# INLINE ggather #-}


-- -- === Redirect instances === --

-- instance {-# OVERLAPPABLE #-} GTraversable (Provider tag m) a
--       => Provider tag m a where
--     gather = ggather @tag ; {-# INLINE gather #-}

-- instance {-# OVERLAPPABLE #-} Provider1 tag m a
--       => Provider tag m (a t1) where
--     gather = gather1 @tag ; {-# INLINE gather #-}


-- -- === Std instances === --

-- -- instance Provider tag m Bool
-- -- instance Provider tag m Word8
-- -- instance Provider tag m Word64
-- -- instance Provider tag m SomePtr

-- -- instance {-# OVERLAPPABLE #-} Provider1 tag m (Component tag')
-- -- instance Functor m
-- --       => Provider1 tag m (Component tag) where
-- --     gather1 = \a acc -> (Layout.relayout a :) <$> acc ; {-# INLINE gather1 #-}

-- -- instance {-# OVERLAPPABLE #-} Provider1 tag m (Component tag')
-- instance Fold.Foldable1 (ComponentDiscovery comp) m (Component comp')
--       => Provider1 comp m (Component comp') where
--     gather1 = Fold.buildFold1 @(ComponentDiscovery comp) ; {-# INLINE gather1 #-}



-- instance Fold.Foldable (ComponentDiscovery comp) m (Foreign.Vector a)
--       => Provider comp m (Foreign.Vector a) where
--     gather = Fold.buildFold @(ComponentDiscovery comp) ; {-# INLINE gather #-}


-- instance Fold.Foldable (ComponentDiscovery comp) m (PtrList.UnmanagedPtrList a)
--       => Provider comp m (PtrList.UnmanagedPtrList a) where
--     gather = Fold.buildFold @(ComponentDiscovery comp) ; {-# INLINE gather #-}

-- instance Fold.Foldable (ComponentDiscovery comp) m (PtrSet.UnmanagedPtrSet a)
--       => Provider comp m (PtrSet.UnmanagedPtrSet a) where
--     gather = Fold.buildFold @(ComponentDiscovery comp) ; {-# INLINE gather #-}


-- instance Fold.Foldable1 (ComponentDiscovery comp) m (Set a)
--       => Provider1 comp m (Set a) where
--     gather1 = Fold.buildFold1 @(ComponentDiscovery comp) ; {-# INLINE gather1 #-}





-- instance Provider tag m t
--       => Provider1 tag m (Layer.Simple t) where
--     gather1 = \a -> gather @tag (unwrap a) ; {-# INLINE gather1 #-}

-- -- instance {-# OVERLAPPABLE #-} Provider comp m (Foreign.Vector a)
-- -- instance MonadIO m
-- --       => Provider comp m (Foreign.Vector (Component comp layout)) where
-- --     gather = \a acc -> (\a b -> a <> b) <$> (Layout.relayout <<$>> Foreign.toList a)
-- --                                         <*> acc
-- --     {-# INLINE gather #-}

-- -- instance {-# OVERLAPPABLE #-} Provider tag m (PtrList.UnmanagedPtrList a)
-- -- instance MonadIO m
-- --       => Provider tag m (PtrList.UnmanagedPtrList (Component tag layout)) where
-- --     gather = \a acc -> (\a b -> a <> b) <$> (Layout.relayout <<$>> PtrList.toList a)
-- --                                         <*> acc
-- --     {-# INLINE gather #-}

-- -- instance {-# OVERLAPPABLE #-} Provider tag m (PtrSet.UnmanagedPtrSet a)
-- -- instance MonadIO m
-- --       => Provider tag m (PtrSet.UnmanagedPtrSet (Component tag layout)) where
-- --     gather = \a acc -> (\a b -> a <> b) <$> (Layout.relayout <<$>> PtrSet.toList a)
-- --                                         <*> acc
-- --     {-# INLINE gather #-}

-- -- instance Provider  tag m (PtrSet.UnmanagedPtrSet (Component tag' layout))
-- --       => Provider1 tag m (Set tag') where
-- --     gather1 = \a -> gather $! unwrap a ; {-# INLINE gather1 #-}
