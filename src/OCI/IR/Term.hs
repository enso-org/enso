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

-- === Instances === --

-- type instance




type family TermDef a

newtype TermRef a = TermRef (Ptr (TermDef a))
    deriving (Eq, Show, Storable)
