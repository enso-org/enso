module Luna.IR.Component.Link.Class where

import Prologue
import Type.Data.Ord

import qualified OCI.IR.Component      as Component
import qualified OCI.IR.Layer.Internal as Layer
import qualified OCI.IR.Layout         as Layout

import Luna.IR.Component.Term.Class (Term)
import OCI.IR.Layout                ((:=), Layout)



------------------
-- === Link === --
------------------

-- === Definition === ---

Component.define "Link"
type SomeLink = Link ()
type src *-* tgt = Layout '[Source := src, Target := tgt]



--------------------
-- === Layers === --
--------------------

data Source
data Target

type instance Layer.Layout Links Source layout = Layout.Get Source layout
type instance Layer.Layout Links Target layout = Layout.Get Target layout
type instance Layer.Cons   Links Source        = Term
type instance Layer.Cons   Links Target        = Term

type instance Cmp Source Target = 'LT
type instance Cmp Target Source = 'GT



----------------------
-- === HasLinks === --
----------------------


class HasLinks a where
    readLinksIO :: a -> IO [SomeLink]

readLinks :: (HasLinks a, MonadIO m) => a -> m [SomeLink]
readLinks = liftIO . readLinksIO
{-# INLINE readLinks #-}

instance HasLinks (Link a) where

instance HasLinks Int where
    readLinksIO _ = pure mempty ; {-# INLINE readLinksIO #-}
