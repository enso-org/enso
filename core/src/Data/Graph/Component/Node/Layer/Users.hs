{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Component.Node.Layer.Users where

import Prologue hiding (Type)

import qualified Data.Construction               as Data
import qualified Data.Graph.Component.Edge.Class as Link
import qualified Data.Graph.Data.Layer.Class     as Layer
import qualified Data.Graph.Data.Layer.Layout    as Layout
import qualified Data.Mutable.Class              as Mutable

import Data.Graph.Component.Edge.Class       (type (*-*))
import Data.Graph.Component.Node.Layer.Model (Model)
import Data.Graph.Data.Layer.Class           (Layer)



-------------------
-- === Users === --
-------------------

data Users deriving (Generic)
instance Layer  Users where
    type Cons   Users = Link.Set
    type Layout Users layout = layout *-* Layout.Set Model () layout
    manager = Layer.Manager (Just Mutable.new) (Just Data.destructShallow1)

