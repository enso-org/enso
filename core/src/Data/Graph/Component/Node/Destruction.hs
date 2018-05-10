module Data.Graph.Component.Node.Destruction where

import Prologue

import qualified Control.Monad.State.Layered     as State
import qualified Data.Graph.Component.Edge.Class as Edge
import qualified Data.Graph.Data.Component.Class as Component

import Data.Graph.Component.Edge.Class (Edge)
import Data.Graph.Component.Node.Class (Node, Nodes)


-------------------------
-- === Destruction === --
-------------------------

-- === API === --

delete :: ( MonadIO m
          , State.Getter (Edge.ComponentProvider Nodes) m
          , Component.Destructor1 m Edge
          ) => Node layout -> m ()
delete = mapM_ Component.destruct1 <=< Edge.componentEdges ; {-# INLINE delete #-}
