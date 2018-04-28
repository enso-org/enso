{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Term.Layer.Type where

import Prologue hiding (Type)

import qualified Data.Graph.Component.Layer  as Layer
import qualified Data.Graph.Component.Layout as Layout

import Luna.IR.Component.Link.Class (type (*-*), Link)
import Data.Graph.Component.Layer                 (Layer)


------------------
-- === Type === --
------------------

data Type deriving (Generic)
instance Layer  Type where
    type Cons   Type = Link
    type Layout Type layout = Layout.Get Type layout *-* layout
    manager = Layer.unsafeOnlyDestructorManager
type instance Layout.Default Type = ()

