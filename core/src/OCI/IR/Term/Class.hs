module OCI.IR.Term.Class (module OCI.IR.Term.Class, module X) where
import Data.Graph.Component.Node.Class as X (showTag)

import Data.Graph.Component.Node.Class (Node, Nodes, SomeNode)

type Term     = Node
type Terms    = Nodes
type SomeTerm = SomeNode
