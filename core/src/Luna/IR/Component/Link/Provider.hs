{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Link.Provider where

import Prologue

import qualified Data.PtrList.Mutable         as PtrList
import qualified Data.Vector.Storable.Foreign as Foreign
import qualified Luna.IR.Component.Term.Class as Term
import qualified OCI.IR.Layout                as Layout

import Data.Generics.Traversable    (GTraversable, gfoldlM)
import Luna.IR.Component.Link.Class (Link, SomeLink)
import OCI.Data.Name                (Name)



----------------------
-- === Provider === --
----------------------

-- === Definition === --

class Provider  a where
    linksIO :: a -> IO [SomeLink]
    linksIO = const $ pure mempty ; {-# INLINE linksIO #-}

class Provider1 a where
    linksIO1 :: ∀ t1. a t1 -> IO [SomeLink]
    linksIO1 = const $ pure mempty ; {-# INLINE linksIO1 #-}


-- === API === --

links  :: (Provider  a, MonadIO m) => a    -> m [SomeLink]
links1 :: (Provider1 a, MonadIO m) => a t1 -> m [SomeLink]
links  = liftIO . linksIO  ; {-# INLINE links  #-}
links1 = liftIO . linksIO1 ; {-# INLINE links1 #-}

glinks :: (GTraversable Provider a, MonadIO m) => a -> m [SomeLink]
glinks = gfoldlM @Provider (\acc a -> (acc <>) <$> links a) mempty ; {-# INLINE glinks #-}

-- === Redirect instances === --

instance {-# OVERLAPPABLE #-} GTraversable Provider a => Provider a where
    linksIO = glinks ; {-# INLINE linksIO #-}

instance {-# OVERLAPPABLE #-} Provider1 a => Provider (a t1) where
    linksIO = linksIO1 ; {-# INLINE linksIO #-}


-- === Std instances === --

instance Provider Bool
instance Provider Name
instance Provider Word8
instance Provider Word64
instance {-# OVERLAPPABLE #-} Provider (Foreign.Vector a)

instance Provider1 Link where
    linksIO1 = pure . pure . Layout.relayout ; {-# INLINE linksIO1 #-}

instance Provider (PtrList.UnmanagedPtrList (Link t)) where
    linksIO lst = fmap Layout.relayout <$> PtrList.toList lst ; {-# INLINE linksIO #-}

instance GTraversable Provider (Term.Constructor t a)
    => Provider (Term.Constructor t a) where
    linksIO = glinks ; {-# INLINE linksIO #-}
