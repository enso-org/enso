{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Term.Layer.Users where

import Prologue hiding (Type)

import qualified Data.PtrSet.Mutable          as PtrSet
import qualified Data.Set.Mutable.Class       as Set
import qualified Foreign.Ptr                  as Ptr
import qualified Foreign.Storable             as Storable
import qualified Foreign.Storable.Utils       as Storable
import qualified Foreign.Storable1            as Storable1
import qualified Foreign.Storable1.Deriving   as Storable1
import qualified Luna.IR.Component.Link.Class as Link
import qualified Luna.IR.Component.Term.Class as Term
import qualified Luna.IR.Term.Format          as Format
import qualified OCI.IR.Component             as Component
import qualified OCI.IR.Layer                 as Layer
import qualified OCI.IR.Layout                as Layout

import Data.PtrSet.Mutable                (UnmanagedPtrSet)
import Data.Set.Mutable.Class             (Set)
import Foreign.Storable                   (Storable)
import Foreign.Storable1                  (Storable1)
import Luna.IR.Component.Link.Class       (type (*-*), Link, Links)
import Luna.IR.Component.Term.Class       (Term, Terms)
import Luna.IR.Component.Term.Layer.Model (Model, model)
import OCI.IR.Layer                       (Layer)


-------------------
-- === Users === --
-------------------

data Users deriving (Generic)
instance Layer  Users where
    type Cons   Users = Link.Set
    type Layout Users layout = layout *-* Layout.Set Model () layout
    manager = Layer.dynamicManager
