{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Link.Class where

import Prologue

import qualified OCI.IR.Component          as Component
import qualified OCI.IR.Component.Provider as Component
import qualified OCI.IR.Layer              as Layer
import qualified OCI.IR.Layout             as Layout

import Data.PtrSet.Mutable          (IsPtr)
import Foreign.Ptr.Utils            (SomePtr)
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


-- === Provider === --

type Provider  = Component.Provider  Links
type Provider1 = Component.Provider1 Links

links  :: (MonadIO m, Provider  a) => a    -> m [SomeLink]
links1 :: (MonadIO m, Provider1 a) => a t1 -> m [SomeLink]
links  = Component.components  @Links ; {-# INLINE links  #-}
links1 = Component.components1 @Links ; {-# INLINE links1 #-}



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


-- === Instances === --

-- instance Component.Provider1 Link where
--     pointersIO1 = pure . pure . convertTo' @SomePtr


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

-- instance Component.Provider1 Set where
--     pointersIO1 = Component.pointersIO1 . unwrap
