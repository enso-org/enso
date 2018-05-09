{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Component.Edge.Class where

import Prologue

import qualified Control.Monad.State.Layered  as State
import qualified Data.Graph.Data              as Component
import qualified Data.Graph.Data.Layer.Class  as Layer
import qualified Data.Graph.Data.Layer.Layout as Layout

import Data.Graph.Component.Node.Class (Node)
import Data.Graph.Data                 (Component, SomeComponent)
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

edges  :: (MonadIO m, Provider  a) => a    -> m [SomeEdge]
edges1 :: (MonadIO m, Provider1 a) => a t1 -> m [SomeEdge]
edges  = Component.components  @Edges ; {-# INLINE edges  #-}
edges1 = Component.components1 @Edges ; {-# INLINE edges1 #-}


-- === ComponentProvider === --

newtype ComponentProvider tag = ComponentProvider
    (SomeComponent tag -> IO [SomeEdge])

componentEdges
    :: ∀ tag m layout. (MonadIO m, State.Getter (ComponentProvider tag) m)
    => Component tag layout -> m [SomeEdge]
componentEdges cmp = do
    ComponentProvider f <- State.get @(ComponentProvider tag)
    liftIO . f $ coerce cmp
{-# INLINE componentEdges #-}



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

