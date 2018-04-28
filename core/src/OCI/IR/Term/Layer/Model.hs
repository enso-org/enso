{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Term.Layer.Model where

import Prologue

import qualified Data.Construction            as Data
import qualified OCI.IR.Link.Class as Link
import qualified OCI.IR.Term.Class as Term
import qualified Data.Graph.Component.Layer                 as Layer
import qualified Data.Graph.Component.Layout                as Layout

import OCI.IR.Link.Class (SomeLink)
import OCI.IR.Term.Class (Term, Terms)
import Data.Graph.Component.Layer                 (Layer)



-------------------
-- === Model === --
-------------------

-- === Definition === --

data Model deriving (Generic)
instance Data.ShallowDestructor1 IO Term.Uni => Layer Model where
    type Cons  Model        = Term.Uni
    type View  Model layout = Term.TagToCons (Layout.Get Model layout)
    manager = Layer.unsafeOnlyDestructorManager


-- === Utils === --

model :: Layer.ViewReader Terms Model layout m
      => Term layout -> m (Layer.ViewData Model layout)
model = Layer.readView @Model ; {-# INLINE model #-}

inputs :: ( Layer.Reader Terms Model m
          , Layer.IsUnwrapped Term.Uni
          , Link.Provider1    Term.Uni
          , MonadIO m
          ) => Term layout -> m [SomeLink]
inputs = Link.links1 <=< Layer.read @Model ; {-# INLINE inputs #-}


-- === Instances === --

instance (Term.IsUni t, Layer.IsUnwrapped Term.Uni)
      => Layer.IsCons1 Model t where
    cons1 = Term.toUni ; {-# INLINE cons1 #-}
