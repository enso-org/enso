{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Term.Layer where

import Prologue hiding (Type)

import qualified Data.PtrSet.Cpp2             as PtrSet
import qualified Data.Set.Mutable.Class       as Set
import qualified Foreign.Ptr                  as Ptr
import qualified Foreign.Storable             as Storable
import qualified Foreign.Storable.Utils       as Storable
import qualified Foreign.Storable1            as Storable1
import qualified Luna.IR.Component.Term.Class as Term
import qualified Luna.IR.Term.Format          as Format
import qualified OCI.IR.Component             as Component
import qualified OCI.IR.Layer.Internal        as Layer
import qualified OCI.IR.Layout                as Layout

import Data.PtrSet.Cpp2             (IsPtr, UnmanagedPtrSet)
import Data.Set.Mutable.Class       (Set)
import Foreign.Storable             (Storable)
import Foreign.Storable.Deriving    (deriveStorable)
import Foreign.Storable1            (Storable1)
import Foreign.Storable1.Deriving   (deriveStorable1)
import Luna.IR.Component.Link.Class (type (*-*), Link)
import Luna.IR.Component.Term.Class (Term, Terms)


-- TODO: refactor
instance IsPtr (Link l)


-------------------
-- === Model === --
-------------------

data Model
type instance Layer.Layout Terms Model layout = layout
type instance Layer.Data   Terms Model = Term.Uni
instance Term.IsUni t => Layer.DataCons1 Terms Model t where
    consData1 = Term.toUni ; {-# INLINE consData1 #-}



------------------
-- === Type === --
------------------

data Type
type instance Layer.Layout Terms Type layout = Layout.Get Type layout *-* layout
type instance Layer.Data   Terms Type        = Link
type instance Layout.Default Type = ()



-------------------
-- === Users === --
-------------------

-- === Definition === --

data Users
type    UsersSetData layout = UnmanagedPtrSet (Link layout)
newtype UsersSet     layout = UsersSet (UsersSetData layout)
    deriving (Show, Storable)
makeLenses      ''UsersSet
deriveStorable1 ''UsersSet

-- FIXME: change () to Top ?
type instance Layer.Data   Terms Users = UsersSet
type instance Layer.Layout Terms Users layout
   = layout *-* Layout.Set Model () layout


-- === Instances === --

type instance Set.Item (UsersSet layout) = Set.Item (UsersSetData layout)
instance MonadIO m => Set m (UsersSet l) where
    new    = wrap <$> Set.new    ; {-# INLINE new    #-}
    insert = Set.insert . unwrap ; {-# INLINE insert #-}
    delete = Set.delete . unwrap ; {-# INLINE delete #-}
    member = Set.member . unwrap ; {-# INLINE member #-}
    size   = Set.size   . unwrap ; {-# INLINE size   #-}
    null   = Set.null   . unwrap ; {-# INLINE null   #-}
    toList = Set.toList . unwrap ; {-# INLINE toList #-}

instance Layer.DataInitializer UsersSet where
    initData = wrap PtrSet.new' ; {-# INLINE initData #-}

