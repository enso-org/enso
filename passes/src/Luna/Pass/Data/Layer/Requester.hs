{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Data.Layer.Requester where

import Prologue

import qualified Data.Construction               as Data
import qualified Data.Graph.Data.Layer.Layout    as Layout
import qualified Data.Graph.Data.Layer.Class     as Layer

import Data.Graph.Data.Component.Maybe       (MaybeComponent)
import Data.Graph.Data.Layer.Class           (Layer)
import Data.Graph.Component.Edge.Class       (type (*-*))
import Data.Graph.Component.Node.Layer.Model (Model)
import Data.Graph.Component.Edge.Class       (Edges)

data Requester deriving (Generic)
instance Layer Requester where
    type Cons   Requester        = MaybeComponent Edges
    type Layout Requester layout = Layout.Set Model () layout *-* layout
    manager = Layer.staticManager

