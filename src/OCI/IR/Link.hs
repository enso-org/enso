module OCI.IR.Link where

import Foreign          (Ptr)
import Foreign.Storable (Storable)
import Prologue

import OCI.IR.Term

import qualified Data.Tag               as Tag
import           Foreign                (castPtr)
import           Foreign.Storable.Utils
import           OCI.IR.Component
import           Type.Data.Ord


type family IRDef a


------------------
-- === Link === --
------------------

-- === Definition === ---

componentInstance "Link"
type SomeLink = Link ()


data Source
data Target


data src :-: tgt

type instance Cmp Source Target = 'LT
type instance Cmp Target Source = 'GT

