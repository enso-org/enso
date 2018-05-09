{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Component.Edge.Class where

import Prologue

import qualified Data.Graph.Data              as Component
import qualified Data.Graph.Data.Layer.Class  as Layer
import qualified Data.Graph.Data.Layer.Layout as Layout

import Data.Graph.Component.Node.Class (Node)
import Data.Graph.Data                 (SomeComponent)
import Data.Graph.Data.Layer.Class     (Layer)
import Data.Graph.Data.Layer.Layout    ((:=), Layout)



------------------
-- === Edge === --
------------------

-- === Definition === ---

Component.define "Edge"
type SomeEdge = SomeComponent Edges
type src *-* tgt = Layout '[Source := src, Target := tgt]


-- === Provider === --

type Provider  = Component.Provider  Edges
type Provider1 = Component.Provider1 Edges

links  :: (MonadIO m, Provider  a) => a    -> m [SomeEdge]
links1 :: (MonadIO m, Provider1 a) => a t1 -> m [SomeEdge]
links  = Component.components  @Edges ; {-# INLINE links  #-}
links1 = Component.components1 @Edges ; {-# INLINE links1 #-}



--------------------
-- === Layers === --
--------------------

-- === Definition === --

data Source deriving (Generic)
instance Layer  Source where
    type Cons   Source = Node
    type Layout Source layout = Layout.Get Source layout
    manager = Layer.unsafeOnlyDestructorManager

data Target deriving (Generic)
instance Layer  Target where
    type Cons   Target        = Node
    type Layout Target layout = Layout.Get Target layout
    manager = Layer.unsafeOnlyDestructorManager


-- === Helpers === --

source :: Layer.Reader Edge Source m
       => Edge layout -> m (Node (Layout.Get Source layout))
source = Layer.read @Source ; {-# INLINE source #-}

target :: Layer.Reader Edge Target m
       => Edge layout -> m (Node (Layout.Get Target layout))
target = Layer.read @Target ; {-# INLINE target #-}



------------------------
-- === Components === --
------------------------

type Set = Component.Set Edges
