{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Term.Layer.Type where

import Prologue hiding (Type)

import qualified Data.Graph.Component.Layer.Class  as Layer
import qualified Data.Graph.Component.Layer.Layout as Layout

import OCI.IR.Link.Class (type (*-*), Link)
import Data.Graph.Component.Layer.Class                 (Layer)


------------------
-- === Type === --
------------------

data Type deriving (Generic)
instance Layer  Type where
    type Cons   Type = Link
    type Layout Type layout = Layout.Get Type layout *-* layout
    manager = Layer.unsafeOnlyDestructorManager
type instance Layout.Default Type = ()

