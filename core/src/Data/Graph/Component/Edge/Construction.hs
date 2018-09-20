module Data.Graph.Component.Edge.Construction where

import Prologue

import qualified Data.Graph.Data              as Component
import qualified Data.Graph.Data.Layer.Class  as Layer
import qualified Data.Graph.Data.Layer.Layout as Layout
import qualified Data.Mutable.Class           as Mutable

import Data.Graph.Component.Edge.Class
import Data.Graph.Component.Node.Class (Node)
import Data.Graph.Component.Node.Layer (Users)



------------------
-- === Edge === --
------------------

-- === Construction === --

type Creator m =
    ( Component.Creator Edges m
    , Layer.Writer Edge Source m
    , Layer.Writer Edge Target m
    , Layer.Editor Node Users  m
    )

new :: Creator m => Node src -> Node tgt -> m (Edge (src *-* tgt))
new src tgt = do
    link    <- Component.construct'
    userMap <- Layer.read @Users src
    Mutable.insert userMap (Layout.unsafeRelayout link)
    Layer.write @Source link src
    Layer.write @Target link tgt
    pure link
{-# INLINE new #-}

