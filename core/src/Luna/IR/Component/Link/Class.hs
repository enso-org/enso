{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Link.Class where

import Prologue
import Type.Data.Ord

import qualified OCI.IR.Component as Component
import qualified OCI.IR.Layer     as Layer
import qualified OCI.IR.Layout    as Layout

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

-- === Definition === --

data Source
type instance Layer.Cons   Source        = Term
type instance Layer.Layout Source layout = Layout.Get Source layout
instance Layer.Initializer Source

data Target
type instance Layer.Cons   Target        = Term
type instance Layer.Layout Target layout = Layout.Get Target layout
instance Layer.Initializer Target

type instance Cmp Source Target = 'LT
type instance Cmp Target Source = 'GT


-- === Helpers === --

source :: Layer.Reader Links Source m => Link layout -> m (Term (Layout.Get Source layout))
target :: Layer.Reader Links Target m => Link layout -> m (Term (Layout.Get Target layout))
source = Layer.read @Source ; {-# INLINE source #-}
target = Layer.read @Target ; {-# INLINE target #-}



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



------------------------
-- === Components === --
------------------------

type Set = Component.Set Links
