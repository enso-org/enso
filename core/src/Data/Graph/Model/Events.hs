module Data.Graph.Model.Events where

import Prologue
import Data.Graph.Model.Node
import Data.Graph.Model.Cluster
import Data.Graph.Model.Ptr

data ELEMENT           = ELEMENT           deriving (Show)

data NODE_REMOVE       = NODE_REMOVE       deriving (Show)

data CONNECTION        = CONNECTION        deriving (Show)
data CONNECTION_REMOVE = CONNECTION_REMOVE deriving (Show)

data SUBGRAPH_INCLUDE  = SUBGRAPH_INCLUDE  deriving (Show)
data SUBGRAPH_EXCLUDE  = SUBGRAPH_EXCLUDE  deriving (Show)

data SubgraphElemEvent re rc = SubgraphElemEvent { _elem    :: re
                                                 , _cluster :: rc
                                                 } deriving (Show, Eq)
