{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Term.Layer where

import Prologue hiding (Type)

import qualified Foreign.Ptr                  as Ptr
import qualified Foreign.Storable             as Storable
import qualified Foreign.Storable.Utils       as Storable
import qualified Foreign.Storable1            as Storable1
import qualified Luna.IR.Component.Term.Class as Term
import qualified Luna.IR.Term.Format          as Format
import qualified OCI.IR.Component             as Component
import qualified OCI.IR.Layer.Internal        as Layer
import qualified OCI.IR.Layout                as Layout

import Foreign.Storable             (Storable)
import Foreign.Storable.Deriving    (deriveStorable)
import Foreign.Storable1            (Storable1)
import Foreign.Storable1.Deriving   (deriveStorable1)
import Luna.IR.Component.Link       (type (*-*), Link)
import Luna.IR.Component.Term.Class (Term, Terms)
import OCI.IR.Conversion            (cast)


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

-- newtype MaybeType a = MaybeType (Maybe (Link (Layout.Get Type a *-* a)))
--     deriving (Show)

-- instance Storable (Maybe (Link a)) where
--     sizeOf    _   = Storable.sizeOf'    @(Link a)
--     alignment _   = Storable.alignment' @(Link a)
--     peek      ptr = Storable.peek (coerce ptr) >>= \case
--         False -> pure Nothing
--         True  -> Storable.peekByteOff ptr (Storable.sizeOf' @Bool)
--     poke      ptr = \case
--         Nothing -> Storable.poke (coerce ptr) False
--         Just a  -> Storable.poke (coerce ptr) True
--                 >> Storable.pokeByteOff ptr (Storable.sizeOf' @Bool) a
--     {-# INLINE sizeOf    #-}
--     {-# INLINE alignment #-}
--     {-# INLINE peek      #-}
--     {-# INLINE poke      #-}

-- makeLenses      ''MaybeType
-- deriveStorable  ''MaybeType
-- deriveStorable1 ''MaybeType

-- instance Layer.DataInitializer MaybeType where
--     initData = wrap Nothing ; {-# INLINE initData #-}


