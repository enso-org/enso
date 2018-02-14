{-# LANGUAGE TypeInType #-}

module OCI.IR.Term where

import Prologue
import Foreign          (Ptr)
import Foreign.Storable (Storable)


import Data.Tag

------------------
-- === Term === --
------------------

-- === Definition === --

data TERM
type TermTag = Tag TERM
--
-- === Instances === --

-- type instance




-- type family TermDef a
--
-- newtype TermRef a = TermRef (Ptr (TermDef a))
--     deriving (Eq, Show, Storable)


newtype MData (layout :: k) = MData (Ptr()) deriving (Eq, Show, Storable)

class MutableData a where
    type family DataLayout a
    mdata :: Iso' a (MData (DataLayout a))

newtype IR (t :: Type) = IR (MData t) deriving (Eq, Show, Storable)
makeLenses ''IR

instance MutableData (IR t) where
    type DataLayout  (IR t) = t
    mdata = wrapped ; {-# INLINE mdata #-}
