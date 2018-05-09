{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Term.Layer.Users where

import Prologue hiding (Type)

import qualified Data.Graph.Data.Layer.Class  as Layer
import qualified Data.Graph.Data.Layer.Layout as Layout
import qualified OCI.IR.Link.Class           as Link

import Data.Graph.Data.Layer.Class (Layer)
import OCI.IR.Link.Class          (type (*-*))
import OCI.IR.Term.Layer.Model    (Model)



-------------------
-- === Users === --
-------------------

data Users deriving (Generic)
instance Layer  Users where
    type Cons   Users = Link.Set
    type Layout Users layout = layout *-* Layout.Set Model () layout
    manager = Layer.dynamicManager
