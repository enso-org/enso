{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Link.Class where

import Prologue

import qualified Data.Graph.Component          as Component
import qualified Data.Graph.Component.Layer    as Layer
import qualified Data.Graph.Component.Layout   as Layout
import qualified Data.Graph.Component.Provider as Component

import Data.Graph.Component         (SomeComponent)
import Data.Graph.Component.Layer   (Layer)
import Data.Graph.Component.Layout  ((:=), Layout)
import Data.PtrSet.Mutable          (IsPtr)
import Foreign.Ptr.Utils            (SomePtr)
import Luna.IR.Component.Term.Class (Term)




------------------
-- === Link === --
------------------

-- === Definition === ---

Component.define "Link"
type SomeLink = SomeComponent Links
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
