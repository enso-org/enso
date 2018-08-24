{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Component.Node.Layer.Type where

import Prologue hiding (Type)

import qualified Data.Graph.Data.Layer.Class  as Layer
import qualified Data.Graph.Data.Layer.Layout as Layout

import Data.Graph.Component.Edge.Class (type (*-*), Edge)
import Data.Graph.Data.Layer.Class     (Layer)


------------------
-- === Type === --
------------------

data Type deriving (Generic)
instance Layer  Type where
    type Cons   Type = Edge
    type Layout Type layout = Layout.Get Type layout *-* layout
    manager = Layer.unsafeOnlyDestructorManager
type instance Layout.Default Type = ()

