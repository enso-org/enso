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
import qualified Foreign.Storable1.Deriving   as Storable1
import qualified Luna.IR.Component.Term.Class as Term
import qualified Luna.IR.Term.Format          as Format
import qualified OCI.IR.Component             as Component
import qualified OCI.IR.Layer.Internal        as Layer
import qualified OCI.IR.Layout                as Layout

import Data.PtrSet.Cpp2             (IsPtr, UnmanagedPtrSet)
import Data.Set.Mutable.Class       (Set)
import Foreign.Storable             (Storable)
import Foreign.Storable1            (Storable1)
import Luna.IR.Component.Link.Class (type (*-*), Link)
import Luna.IR.Component.Term.Class (Term, Terms)


-- TODO: refactor
instance IsPtr (Link l)


-------------------
-- === Model === --
-------------------

data Model
type instance Layer.Cons   Terms Model = Term.Uni
type instance Layer.Layout Terms Model layout = layout
instance Term.IsUni t => Layer.IsCons1 Terms Model t where
    cons1 = Term.toUni ; {-# INLINE cons1 #-}



------------------
-- === Type === --
------------------

data Type
type instance Layer.Cons   Terms Type        = Link
type instance Layer.Layout Terms Type layout = Layout.Get Type layout *-* layout
type instance Layout.Default Type = ()



-------------------
-- === Users === --
-------------------

-- === Definition === --

data Users
type    UsersSetData layout = UnmanagedPtrSet (Link layout)
newtype UsersSet     layout = UsersSet (UsersSetData layout)
    deriving (Show, Storable)
makeLenses       ''UsersSet
Storable1.derive ''UsersSet

-- FIXME: change () to Top ?
type instance Layer.Cons   Terms Users = UsersSet
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
    initStaticData  = Just $ coerce Ptr.nullPtr ; {-# INLINE initStaticData #-}
    initDynamicData = Just Set.new ; {-# INLINE initDynamicData #-}
