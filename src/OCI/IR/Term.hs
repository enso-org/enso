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

--
-- === Instances === --

-- type instance


-- data COMPONENT
-- type ComponentTag = Tag COMPONENT

newtype Component t cfg = Component (Ptr()) deriving (Eq, Show, Storable)




data TERM
type TermTag = Tag TERM
type Term = Component TERM


-- newtype Component = Component (Ptr()) deriving (Eq, Show, Storable)

-- class IsComponent a where
    -- component :: Iso' a Component



-- data IR_COMPONENT
--
--
--
--
-- newtype IR (t :: Type) = IR Component deriving (Eq, Show, Storable)
-- makeLenses ''IR

-- instance IsComponent (IR t) where
    -- component = wrapped ; {-# INLINE component #-}
