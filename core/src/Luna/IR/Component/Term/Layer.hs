{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Term.Layer where

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

import Data.PtrSet.Mutable          (UnmanagedPtrSet)
import Data.Set.Mutable.Class       (Set)
import Foreign.Storable             (Storable)
import Foreign.Storable1            (Storable1)
import Luna.IR.Component.Link.Class (type (*-*), Link, Links)
import Luna.IR.Component.Term.Class (Term, Terms)
import OCI.IR.Layer                 (Layer)



-------------------
-- === Model === --
-------------------

-- === Definition === --

data Model
type instance Layer.Cons     Model        = Term.Uni
type instance Layer.Layout   Model layout = layout
type instance Layer.ViewCons Model layout
   = Term.TagToCons (Layout.Get Model layout)
instance Layer Model


-- === Utils === --

model :: Layer.ViewReader Terms Model layout m
      => Term layout -> m (Layer.View Model layout)
model = Layer.readView @Model ; {-# INLINE model #-}


-- === Instances === --

instance (Term.IsUni t, Layer.IsUnwrapped Term.Uni)
      => Layer.IsCons1 Model t where
    cons1 = Term.toUni ; {-# INLINE cons1 #-}



------------------
-- === Type === --
------------------

data Type
type instance Layout.Default Type = ()
type instance Layer.Cons     Type = Link
type instance Layer.Layout   Type layout = Layout.Get Type layout *-* layout
instance      Layer          Type



-------------------
-- === Users === --
-------------------

data Users
type instance Layer.Cons   Users = Link.Set
type instance Layer.Layout Users layout = layout *-* Layout.Set Model () layout
instance      Layer        Users where
    construct = Just (wrap <$> PtrSet.new) ; {-# INLINE construct #-}

