{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Term.Layout where

import qualified Data.Graph.Component.Node.Class as Node
import qualified Data.Graph.Data.Layer.Layout    as Layout

import Data.Graph.Component.Node.Class       (Nodes)
import Data.Graph.Component.Node.Layer.Model (Model)
import Data.Graph.Data.Layer.Layout          ((:=), Layout)


data Names

type instance Layout.ToLayout (Node.NodeTag a) = Layout '[Model := (Node.NodeTag a)]

type layout -* term = Layout.Set Nodes term (Layout.ToLayout layout)
type layout -# name = Layout.Set Names name (Layout.ToLayout layout)

