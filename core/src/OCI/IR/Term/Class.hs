module OCI.IR.Term.Class (module OCI.IR.Term.Class, module X) where

import Data.Graph.Component.Node.Class as X (showTag)

import qualified Data.Graph.Component.Node.Class as Node

import Data.Graph.Component.Node.Class (Node, Nodes)

type Term     = Node
type Terms    = Nodes
type SomeTerm = Node.Some

