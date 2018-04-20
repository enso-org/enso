{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Link.Class where

import Prologue
import Type.Data.Ord

import qualified OCI.IR.Component as Component
import qualified OCI.IR.Layer     as Layer
import qualified OCI.IR.Layout    as Layout

import Luna.IR.Component.Term.Class (Term)
import OCI.IR.Layer                 (Layer)
import OCI.IR.Layout                ((:=), Layout)



------------------
-- === Link === --
------------------

-- === Definition === ---

Component.define "Link"
type SomeLink = Link ()
type src *-* tgt = Layout '[Source := src, Target := tgt]


-- === Inputs === --

class HasInputs (a :: Type -> Type) where
    inputsIO :: ∀ t. a t -> IO [SomeLink]

inputs :: (HasInputs a, MonadIO m) => a t -> m [SomeLink]
inputs = liftIO . inputsIO ; {-# INLINE inputs #-}



--------------------
-- === Layers === --
--------------------

-- === Definition === --

data Source
instance Layer  Source where
    type Cons   Source = Term
    type Layout Source layout = Layout.Get Source layout
    manager = Layer.unsafeOnlyDestructorManager

data Target
instance Layer  Target where
    type Cons   Target        = Term
    type Layout Target layout = Layout.Get Target layout
    manager = Layer.unsafeOnlyDestructorManager

type instance Cmp Source Target = 'LT
type instance Cmp Target Source = 'GT


-- === Helpers === --

source :: Layer.Reader Links Source m => Link layout -> m (Term (Layout.Get Source layout))
target :: Layer.Reader Links Target m => Link layout -> m (Term (Layout.Get Target layout))
source = Layer.read @Source ; {-# INLINE source #-}
target = Layer.read @Target ; {-# INLINE target #-}



------------------------
-- === Components === --
------------------------

type Set = Component.Set Links
