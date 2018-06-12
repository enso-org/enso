module Data.Graph.Component.Node.Destruction where

import Prologue

import qualified Data.Graph.Component.Edge.Destruction as Edge
import qualified Data.Graph.Data.Component.Class       as Component
import qualified Data.Graph.Data.Component.List        as ComponentList
import qualified Data.Graph.Data.Layer.Class           as Layer

import Data.Graph.Fold.SubComponents         (SubComponents, subComponents)
import Data.Graph.Component.Edge.Class       (Source, Edge, Edges)
import Data.Graph.Component.Node.Class       (Node, Nodes)
import Data.Graph.Component.Node.Layer.Users (Users)


-------------------------
-- === Destruction === --
-------------------------

-- === API === --

delete ::
    ( MonadIO m
    , Edge.Delete m
    , Component.Destructor1 m Node
    , SubComponents Edges m (Node layout)
    ) => Node layout -> m ()
delete = \node -> do
    edges <- subComponents @Edges node
    ComponentList.mapM_ Edge.delete edges
    Component.destruct1 node
{-# INLINE delete #-}
