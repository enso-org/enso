{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Term.Layer.Users where

import Prologue hiding (Type)

import qualified Luna.IR.Component.Link.Class as Link
import qualified Data.Graph.Component.Layer                 as Layer
import qualified Data.Graph.Component.Layout                as Layout

import Luna.IR.Component.Link.Class       (type (*-*))
import Luna.IR.Component.Term.Layer.Model (Model)
import Data.Graph.Component.Layer                       (Layer)


-------------------
-- === Users === --
-------------------

data Users deriving (Generic)
instance Layer  Users where
    type Cons   Users = Link.Set
    type Layout Users layout = layout *-* Layout.Set Model () layout
    manager = Layer.dynamicManager
