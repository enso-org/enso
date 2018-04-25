{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Component.Provider where

import Prologue

import qualified Data.PtrList.Mutable         as PtrList
import qualified Data.PtrSet.Mutable          as PtrSet
import qualified Data.Vector.Storable.Foreign as Foreign
import qualified Foreign.Storable1            as Storable1

import Data.Generics.Traversable (GTraversable, gfoldlM)
import Data.PtrList.Mutable      (IsPtr)
import Foreign.Ptr.Utils         (SomePtr)
import Foreign.Storable1         (Storable1)
import OCI.Data.Name             (Name)


----------------------
-- === Provider === --
----------------------

-- === Definition === --

class Provider  a where
    pointersIO :: a -> IO [SomePtr]
    pointersIO = const $ pure mempty ; {-# INLINE pointersIO #-}

class Provider1 a where
    pointersIO1 :: ∀ t1. a t1 -> IO [SomePtr]
    pointersIO1 = const $ pure mempty ; {-# INLINE pointersIO1 #-}


-- === API === --

type DynamicGetter = SomePtr -> IO [SomePtr]

pointers  :: (Provider  a, MonadIO m) => a    -> m [SomePtr]
pointers1 :: (Provider1 a, MonadIO m) => a t1 -> m [SomePtr]
pointers  = liftIO . pointersIO  ; {-# INLINE pointers  #-}
pointers1 = liftIO . pointersIO1 ; {-# INLINE pointers1 #-}

gpointers :: (GTraversable Provider a, MonadIO m) => a -> m [SomePtr]
gpointers = gfoldlM @Provider (\acc a -> (acc <>) <$> pointers a) mempty ; {-# INLINE gpointers #-}

makeDynamicGetter :: Storable1 t => (t a -> IO [SomePtr]) -> DynamicGetter
makeDynamicGetter getterFun ptr = getterFun =<< Storable1.peek (coerce ptr)


-- === Redirect instances === --

instance {-# OVERLAPPABLE #-} GTraversable Provider a => Provider a where
    pointersIO = gpointers ; {-# INLINE pointersIO #-}

instance {-# OVERLAPPABLE #-} Provider1 a => Provider (a t1) where
    pointersIO = pointersIO1 ; {-# INLINE pointersIO #-}


-- === Std instances === --

instance Provider Bool
instance Provider Name
instance Provider Word8
instance Provider Word64
instance Provider SomePtr
instance {-# OVERLAPPABLE #-} Provider (Foreign.Vector a)

instance IsPtr a => Provider (PtrList.UnmanagedPtrList a) where
    pointersIO lst = convertTo' @SomePtr <<$>> PtrList.toList lst
    {-# INLINE pointersIO #-}

instance IsPtr a => Provider (PtrSet.UnmanagedPtrSet a) where
    pointersIO s = convertTo' @SomePtr <<$>> PtrSet.toList s
    {-# INLINE pointersIO #-}

instance Provider1 PtrSet.UnmanagedPtrSet where
    pointersIO1 s = do
        let ptrS = unsafeCoerce s :: PtrSet.UnmanagedPtrSet SomePtr
        PtrSet.toList ptrS
