{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Data.Component.Provider where

import Prologue

import qualified Data.Generics.Traversable         as GTraversable
import qualified Data.Graph.Data.Component.Class   as Component
import qualified Data.Graph.Data.Component.Dynamic as Component
import qualified Data.Graph.Data.Layer.Class       as Layer
import qualified Data.Graph.Data.Layer.Layout      as Layout
import qualified Data.PtrList.Mutable              as PtrList
import qualified Data.PtrSet.Mutable               as PtrSet
import qualified Data.Vector.Storable.Foreign      as Foreign

import Data.Generics.Traversable       (GTraversable)
import Data.Graph.Data.Component.Class (Component)
import Data.Map.Strict                 (Map)
import Foreign.Ptr.Utils               (SomePtr)



----------------------
-- === Provider === --
----------------------

-- === Definition === --

class Provider tag m a where
    gather :: a -> m [Component.Some tag] -> m [Component.Some tag]
    gather _ a = a ; {-# INLINE gather #-}

class Provider1 tag m a where
    gather1 :: ∀ t. a t -> m [Component.Some tag] -> m [Component.Some tag]
    gather1 _ a = a ; {-# INLINE gather1 #-}


-- === API === --

-- gather  :: ∀ tag a m. (MonadIO m, Provider tag a)
--             => a -> m [Component.Some tag]
-- gather  = liftIO . gather ; {-# INLINE gather #-}

-- gather1 :: ∀ tag a m t1. (MonadIO m, Provider1 tag a)
--             => a t1 -> m [Component.Some tag]
-- gather1 = liftIO . gather1 ; {-# INLINE gather1 #-}

ggather :: ∀ tag m a. (GTraversable (Provider tag m) a)
        => a -> m [Component.Some tag] -> m [Component.Some tag]
ggather = GTraversable.gfoldl' @(Provider tag m) (\f a x -> f $! gather @tag a x) (\a -> a)
{-# INLINE ggather #-}


-- === Redirect instances === --

instance {-# OVERLAPPABLE #-} GTraversable (Provider tag m) a
      => Provider tag m a where
    gather = ggather @tag ; {-# INLINE gather #-}

instance {-# OVERLAPPABLE #-} Provider1 tag m a
      => Provider tag m (a t1) where
    gather = gather1 @tag ; {-# INLINE gather #-}


-- === Std instances === --

instance Provider tag m Bool
instance Provider tag m Word8
instance Provider tag m Word64
instance Provider tag m SomePtr

instance {-# OVERLAPPABLE #-} Provider1 tag m (Component tag')
instance Functor m
      => Provider1 tag m (Component tag) where
    gather1 a acc = (Layout.relayout a :) <$> acc ; {-# INLINE gather1 #-}

instance Provider tag m t
      => Provider1 tag m (Layer.Simple t) where
    gather1 a = gather @tag (unwrap a) ; {-# INLINE gather1 #-}

instance {-# OVERLAPPABLE #-} Provider tag m (Foreign.Vector a)
instance MonadIO m
      => Provider tag m (Foreign.Vector (Component tag layout)) where
    gather a acc = (\a b -> a <> b) <$> (Layout.relayout <<$>> Foreign.toList a) <*> acc
    {-# INLINE gather #-}

instance {-# OVERLAPPABLE #-} Provider tag m (PtrList.UnmanagedPtrList a)
instance MonadIO m
      => Provider tag m (PtrList.UnmanagedPtrList (Component tag layout)) where
    gather a acc = (\a b -> a <> b) <$> (Layout.relayout <<$>> PtrList.toList a) <*> acc
    {-# INLINE gather #-}

instance {-# OVERLAPPABLE #-} Provider tag m (PtrSet.UnmanagedPtrSet a)
instance MonadIO m
      => Provider tag m (PtrSet.UnmanagedPtrSet (Component tag layout)) where
    gather a acc = (\a b -> a <> b) <$> (Layout.relayout <<$>> PtrSet.toList a) <*> acc
    {-# INLINE gather #-}



-- -----------------------------
-- -- === DynamicProvider === --
-- -----------------------------

-- -- === Definition === --

-- class DynamicProvider a where
--     dynamicComponentsIO :: a -> IO [Component.Dynamic]
--     dynamicComponentsIO = const $ pure mempty ; {-# INLINE dynamicComponentsIO #-}

-- class DynamicProvider1 a where
--     dynamicComponentsIO1 :: ∀ t1. a t1 -> IO [Component.Dynamic]
--     dynamicComponentsIO1 = const $ pure mempty ; {-# INLINE dynamicComponentsIO1 #-}


-- -- === API === --

-- dynamicComponents :: ∀ a m. (MonadIO m, DynamicProvider a)
--                   => a -> m [Component.Dynamic]
-- dynamicComponents = liftIO . dynamicComponentsIO ; {-# INLINE dynamicComponents #-}

-- dynamicComponents1 :: ∀ a m t1. (MonadIO m, DynamicProvider1 a)
--                    => a t1 -> m [Component.Dynamic]
-- dynamicComponents1 = liftIO . dynamicComponentsIO1 ; {-# INLINE dynamicComponents1 #-}

-- gdynamicComponents :: ∀ a m. (GTraversable DynamicProvider a, MonadIO m)
--                    => a -> m [Component.Dynamic]
-- gdynamicComponents = gfoldlM @DynamicProvider (\acc a -> (acc <>) <$> dynamicComponents a)
--               mempty
-- {-# INLINE gdynamicComponents #-}


-- -- === Redirect instances === --

-- instance {-# OVERLAPPABLE #-} GTraversable DynamicProvider a => DynamicProvider a where
--     dynamicComponentsIO = gdynamicComponents ; {-# INLINE dynamicComponentsIO #-}

-- instance {-# OVERLAPPABLE #-} DynamicProvider1 a => DynamicProvider (a t1) where
--     dynamicComponentsIO = dynamicComponentsIO1 ; {-# INLINE dynamicComponentsIO #-}


-- -- === Std instances === --

-- instance DynamicProvider Bool
-- instance DynamicProvider Word8
-- instance DynamicProvider Word64
-- instance DynamicProvider SomePtr

-- instance Typeable tag => DynamicProvider1 (Component tag) where
--     dynamicComponentsIO1 = pure . pure . Component.toDynamic1
--     {-# INLINE dynamicComponentsIO1 #-}

-- instance DynamicProvider t => DynamicProvider1 (Layer.Simple t) where
--     dynamicComponentsIO1 = dynamicComponentsIO . unwrap ; {-# INLINE dynamicComponentsIO1 #-}

-- instance {-# OVERLAPPABLE #-}
--          DynamicProvider (Foreign.Vector a)
-- instance Typeable tag
--       => DynamicProvider (Foreign.Vector (Component tag layout)) where
--     dynamicComponentsIO a = Component.toDynamic1 <<$>> Foreign.toList a ; {-# INLINE dynamicComponentsIO #-}

-- instance Typeable tag
--       => DynamicProvider (PtrList.UnmanagedPtrList (Component tag layout)) where
--     dynamicComponentsIO a = Component.toDynamic1 <<$>> PtrList.toList a ; {-# INLINE dynamicComponentsIO #-}

-- instance Typeable tag
--       => DynamicProvider (PtrSet.UnmanagedPtrSet (Component tag layout)) where
--     dynamicComponentsIO a = Component.toDynamic1 <<$>> PtrSet.toList a ; {-# INLINE dynamicComponentsIO #-}



------------------------------
-- === DynamicTraversal === --
------------------------------

-- type DynamicTraversalSig = SomePtr -> IO [Component.Dynamic]
-- newtype DynamicTraversal comp = DynamicTraversal DynamicTraversalSig
-- makeLenses ''DynamicTraversal

-- newtype DynamicTraversalMap = DynamicTraversalMap
--     (Map Component.TagRep DynamicTraversalSig)
-- makeLenses ''DynamicTraversalMap

