module Luna.IR.Component.Link.Discovery where

import Prologue
import Data.PtrList.Mutable         (UnmanagedPtrList)
import Luna.IR.Component.Link.Class (Link, SomeLink)

import qualified Data.PtrList.Mutable as PtrList
import qualified OCI.IR.Layout        as Layout

class PrependLinks a where
    prependLinks :: MonadIO m => a -> [SomeLink] -> m [SomeLink]

instance {-# OVERLAPPABLE #-} PrependLinks a where
    prependLinks = const return
    {-# INLINE prependLinks #-}

instance PrependLinks (Link a) where
    prependLinks t = return . (Layout.relayout t :)
    {-# INLINE prependLinks #-}

instance PrependLinks (UnmanagedPtrList (Link a)) where
    prependLinks t l = (<> l) . fmap Layout.relayout  <$> PtrList.toList t
    {-# INLINE prependLinks #-}

class HasInputs (a :: * -> *) where
    inputs :: forall t m. MonadIO m => a t -> m [SomeLink]
