module Luna.IR.Link where

import Prologue
import Type.Data.Ord

import qualified OCI.IR.Component      as Component
import qualified OCI.IR.Layer.Internal as Layer
import qualified OCI.IR.Layout         as Layout

import Luna.IR.Term      (Term)
import OCI.IR.Conversion (generalize)
import OCI.IR.Layout     ((:=), Layout)



------------------
-- === Link === --
------------------

-- === Definition === ---

Component.define "Link"
type SomeLink = Link ()
type src *-* tgt = Layout '[Source := src, Target := tgt]


-- === Construction === --

type Creator m =
    ( Component.Creator Links m
    , Layer.Setter Links Source m
    , Layer.Setter Links Target m
    )

new :: Creator m => Term src -> Term tgt -> m (Link (src *-* tgt))
new src tgt = do
    ir <- Component.new
    Layer.put @Source ir src
    Layer.put @Target ir tgt
    pure $ ir
{-# INLINE new #-}



--------------------
-- === Layers === --
--------------------

data Source
data Target

type instance Layer.Layout Links Source layout = Layout.Get Source layout
type instance Layer.Layout Links Target layout = Layout.Get Target layout
type instance Layer.Data   Links Source        = Term
type instance Layer.Data   Links Target        = Term

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
    readLinksIO l = pure . pure $ generalize l ; {-# INLINE readLinksIO #-}

instance HasLinks Int where
    readLinksIO _ = pure mempty ; {-# INLINE readLinksIO #-}
