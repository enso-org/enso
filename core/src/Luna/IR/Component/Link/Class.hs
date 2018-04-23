{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Link.Class where

import Prologue

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



--------------------
-- === Layers === --
--------------------

-- === Definition === --

data Source deriving (Generic)
instance Layer  Source where
    type Cons   Source = Term
    type Layout Source layout = Layout.Get Source layout
    manager = Layer.unsafeOnlyDestructorManager

data Target deriving (Generic)
instance Layer  Target where
    type Cons   Target        = Term
    type Layout Target layout = Layout.Get Target layout
    manager = Layer.unsafeOnlyDestructorManager


-- === Helpers === --

source :: Layer.Reader Links Source m
       => Link layout -> m (Term (Layout.Get Source layout))
source = Layer.read @Source ; {-# INLINE source #-}

target :: Layer.Reader Links Target m
       => Link layout -> m (Term (Layout.Get Target layout))
target = Layer.read @Target ; {-# INLINE target #-}



------------------------
-- === Components === --
------------------------

type Set = Component.Set Links
