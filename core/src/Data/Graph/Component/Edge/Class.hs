{-# LANGUAGE Strict #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Component.Edge.Class where

import Prologue

import qualified Data.Graph.Component.Node.Class as Node
import qualified Data.Graph.Data                 as Component
import qualified Data.Graph.Data.Layer.Class     as Layer
import qualified Data.Graph.Data.Layer.Layout    as Layout

import Data.Graph.Component.Node.Class (Node)
import Data.Graph.Data.Component.Set   (ComponentSet)
import Data.Graph.Data.Layer.Class     (Layer)
import Data.Graph.Data.Layer.Layout    ((:=), Layout)



------------------
-- === Edge === --
------------------

-- === Definition === ---

Component.define "Edge"
type SomeEdge = Component.Some Edges
type src *-* tgt = Layout '[Source := src, Target := tgt]



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

-- TODO: make 'generalize' anstraction and simplify relayout here
cyclic :: (Layer.Reader Edge Source m, Layer.Reader Edge Target m)
       => Edge l -> m Bool
cyclic = \edge -> do
    src <- source edge
    tgt <- target edge
    pure $ (Layout.unsafeRelayout src :: Node.Some)
        == (Layout.unsafeRelayout tgt :: Node.Some)
{-# INLINE cyclic #-}


------------------------
-- === Components === --
------------------------

type Set = ComponentSet Edges

