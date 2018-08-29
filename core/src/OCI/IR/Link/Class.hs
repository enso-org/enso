module OCI.IR.Link.Class (module OCI.IR.Link.Class, module X) where

import Data.Graph.Component.Edge as X (type (*-*), Source, Target, source,
                                       target)

import Data.Graph.Component.Edge (Edge, Edges, SomeEdge)

type Link     = Edge
type Links    = Edges
type SomeLink = SomeEdge

