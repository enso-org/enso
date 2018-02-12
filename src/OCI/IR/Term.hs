module OCI.IR.Term where

import Prologue
import Foreign          (Ptr)
import Foreign.Storable (Storable)
import OCI.IR.Layout


import Data.Tag

------------------
-- === Term === --
------------------

-- === Definition === --

data TERM
type TermTag = Tag TERM

-- === Instances === --

-- type instance




type family TermDef (a :: Layout)

newtype TermRef (a :: Layout) = TermRef (Ptr (TermDef a))
    deriving (Eq, Show, Storable)
