module Data.Graph.Model.Class where

import Prologue

import Data.Graph.Model.Node
import Data.Graph.Model.Edge


-- === Attributes === --

data Inputs  = Inputs  deriving (Show)
data Outputs = Outputs deriving (Show)
data Succs   = Succs   deriving (Show)

data ELEMENT    = ELEMENT    deriving (Show)
data CONNECTION = CONNECTION deriving (Show)


-- === Definitions === ---

--type family Collections ts a where
--    Collections '[]       a = ()
--    Collections (t ': ts) a = (Collection t a, Collections ts a)

--class Collection t g where
--    elems :: Lens' g [g # t]


--type Graph = Collections '[Node, Edge]

--class Collections '[Node, Edge] g => Graph g where

--class Referrenced t g where
--    refs :: g -> [Ref t (g # t)]

--    edgeRefs :: g -> [Ref $ Link (Item g)]

