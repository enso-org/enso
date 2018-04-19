module Luna.IR.Component.Link.Discovery where

import Prologue

import qualified Data.PtrList.Mutable as PtrList
import qualified OCI.IR.Layout        as Layout

import Data.PtrList.Mutable         (UnmanagedPtrList)
import Luna.IR.Component.Link.Class (Link, SomeLink)


-- === Definition === --

class PrependLinks a where
    prependLinks :: a -> [SomeLink] -> IO [SomeLink]

class HasInputs (a :: Type -> Type) where
    inputsIO :: forall t. a t -> IO [SomeLink]


-- === API === --

inputs :: (HasInputs a, MonadIO m) => a t -> m [SomeLink]
inputs = liftIO . inputsIO ; {-# INLINE inputs #-}


-- === Instances === --

instance {-# OVERLAPPABLE #-} PrependLinks a where
    prependLinks = const return
    {-# INLINE prependLinks #-}

instance PrependLinks (Link a) where
    prependLinks t = return . (Layout.relayout t :)
    {-# INLINE prependLinks #-}

instance PrependLinks (UnmanagedPtrList (Link a)) where
    prependLinks t l = (<> l) . fmap Layout.relayout  <$> PtrList.toList t
    {-# INLINE prependLinks #-}

