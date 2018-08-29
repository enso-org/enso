module Data.Graph.Component.Edge.Destruction where

import Prologue

import qualified Data.Graph.Data.Component.Class as Component
import qualified Data.Graph.Data.Layer.Class     as Layer
import qualified Data.Graph.Data.Layer.Layout    as Layout
import qualified Data.Mutable.Class              as Mutable

import Data.Graph.Component.Edge.Class       (Edge, Edges, Source)
import Data.Graph.Component.Node.Class       (Node)
import Data.Graph.Component.Node.Layer.Users (Users)

type Delete m =
    ( MonadIO m
    , Component.Destructor1 m Edge
    , Layer.Reader Node Users m
    , Layer.Reader Edge Source m
    )

delete :: Delete m => Edge layout -> m ()
delete = \edge -> do
    srcUsers <- Layer.read @Users =<< Layer.read @Source edge
    Mutable.remove srcUsers $! Layout.unsafeRelayout edge
    Component.destruct1 edge
{-# INLINE delete #-}

