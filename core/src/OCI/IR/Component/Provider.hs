{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Component.Provider where

import Prologue

import qualified Data.PtrList.Mutable         as PtrList
import qualified Data.PtrSet.Mutable          as PtrSet
import qualified Data.Vector.Storable.Foreign as Foreign
import qualified Foreign.Storable1            as Storable1
import qualified Data.Graph.Component.Class       as Component
import qualified OCI.IR.Component.Dynamic     as Component
import qualified OCI.IR.Layout                as Layout

import Data.Generics.Traversable (GTraversable, gfoldlM)
import Data.PtrList.Mutable      (IsPtr)
import Foreign.Ptr.Utils         (SomePtr)
import Foreign.Storable1         (Storable1)
import OCI.Data.Name             (Name)
import Data.Graph.Component.Class    (Component)



----------------------
-- === Provider === --
----------------------

-- === Definition === --

class Provider tag a where
    componentsIO :: a -> IO [Component tag ()]
    componentsIO = const $ pure mempty ; {-# INLINE componentsIO #-}

class Provider1 tag a where
    componentsIO1 :: ∀ t1. a t1 -> IO [Component tag ()]
    componentsIO1 = const $ pure mempty ; {-# INLINE componentsIO1 #-}


-- === API === --

components  :: ∀ tag a m. (MonadIO m, Provider tag a)
            => a -> m [Component tag ()]
components  = liftIO . componentsIO ; {-# INLINE components #-}

components1 :: ∀ tag a m t1. (MonadIO m, Provider1 tag a)
            => a t1 -> m [Component tag ()]
components1 = liftIO . componentsIO1 ; {-# INLINE components1 #-}

gcomponents :: ∀ tag a m. (GTraversable (Provider tag) a, MonadIO m)
            => a -> m [Component tag ()]
gcomponents = gfoldlM @(Provider tag) (\acc a -> (acc <>) <$> components @tag a)
              mempty
{-# INLINE gcomponents #-}


-- === Redirect instances === --

instance {-# OVERLAPPABLE #-} GTraversable (Provider tag) a => Provider tag a where
    componentsIO = gcomponents @tag ; {-# INLINE componentsIO #-}

instance {-# OVERLAPPABLE #-} Provider1 tag a => Provider tag (a t1) where
    componentsIO = componentsIO1 @tag ; {-# INLINE componentsIO #-}


-- === Std instances === --

instance Provider tag Bool
instance Provider tag Name
instance Provider tag Word8
instance Provider tag Word64
instance Provider tag SomePtr

instance {-# OVERLAPPABLE #-}
         Provider1 tag (Component tag')
instance Provider1 tag (Component tag) where
    componentsIO1 = pure . pure . Layout.relayout ; {-# INLINE componentsIO1 #-}

-- FIXME
instance {-# OVERLAPPABLE #-} Provider tag (Foreign.Vector a)

instance {-# OVERLAPPABLE #-}
         Provider tag (PtrList.UnmanagedPtrList a)
instance Provider tag (PtrList.UnmanagedPtrList (Component tag layout)) where
    componentsIO a = Layout.relayout <<$>> PtrList.toList a ; {-# INLINE componentsIO #-}

instance {-# OVERLAPPABLE #-}
         Provider tag (PtrSet.UnmanagedPtrSet a)
instance Provider tag (PtrSet.UnmanagedPtrSet (Component tag layout)) where
    componentsIO a = Layout.relayout <<$>> PtrSet.toList a ; {-# INLINE componentsIO #-}



-----------------------------
-- === DynamicProvider === --
-----------------------------

-- === Definition === --

class DynamicProvider a where
    dynamicComponentsIO :: a -> IO [Component.Dynamic]
    dynamicComponentsIO = const $ pure mempty ; {-# INLINE dynamicComponentsIO #-}

class DynamicProvider1 a where
    dynamicComponentsIO1 :: ∀ t1. a t1 -> IO [Component.Dynamic]
    dynamicComponentsIO1 = const $ pure mempty ; {-# INLINE dynamicComponentsIO1 #-}


-- === API === --

dynamicComponents :: ∀ a m. (MonadIO m, DynamicProvider a)
                  => a -> m [Component.Dynamic]
dynamicComponents = liftIO . dynamicComponentsIO ; {-# INLINE dynamicComponents #-}

dynamicComponents1 :: ∀ a m t1. (MonadIO m, DynamicProvider1 a)
                   => a t1 -> m [Component.Dynamic]
dynamicComponents1 = liftIO . dynamicComponentsIO1 ; {-# INLINE dynamicComponents1 #-}

gdynamicComponents :: ∀ a m. (GTraversable DynamicProvider a, MonadIO m)
                   => a -> m [Component.Dynamic]
gdynamicComponents = gfoldlM @DynamicProvider (\acc a -> (acc <>) <$> dynamicComponents a)
              mempty
{-# INLINE gdynamicComponents #-}


-- === Redirect instances === --

instance {-# OVERLAPPABLE #-} GTraversable DynamicProvider a => DynamicProvider a where
    dynamicComponentsIO = gdynamicComponents ; {-# INLINE dynamicComponentsIO #-}

instance {-# OVERLAPPABLE #-} DynamicProvider1 a => DynamicProvider (a t1) where
    dynamicComponentsIO = dynamicComponentsIO1 ; {-# INLINE dynamicComponentsIO #-}


-- === Std instances === --

instance DynamicProvider Bool
instance DynamicProvider Name
instance DynamicProvider Word8
instance DynamicProvider Word64
instance DynamicProvider SomePtr

instance Typeable tag => DynamicProvider1 (Component tag) where
    dynamicComponentsIO1 = pure . pure . Component.toDynamic1
    {-# INLINE dynamicComponentsIO1 #-}

-- FIXME
instance {-# OVERLAPPABLE #-} DynamicProvider (Foreign.Vector a)

instance Typeable tag
      => DynamicProvider (PtrList.UnmanagedPtrList (Component tag layout)) where
    dynamicComponentsIO a = Component.toDynamic1 <<$>> PtrList.toList a ; {-# INLINE dynamicComponentsIO #-}

instance Typeable tag
      => DynamicProvider (PtrSet.UnmanagedPtrSet (Component tag layout)) where
    dynamicComponentsIO a = Component.toDynamic1 <<$>> PtrSet.toList a ; {-# INLINE dynamicComponentsIO #-}

