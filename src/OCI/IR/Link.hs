module OCI.IR.Link where

import Prologue
import Foreign          (Ptr)
import Foreign.Storable (Storable)

import OCI.IR.Term
import OCI.IR.Layout




type family IRDef a


------------------
-- === Link === --
------------------

-- === Definition === ---

data IRLink src tgt = IRLink
    { _source :: {-# UNPACK #-} !(TermRef src)
    , _target :: {-# UNPACK #-} !(TermRef tgt)
    } deriving (Eq, Show)

newtype LinkRef src tgt = LinkRef (Ptr (IRLink src tgt))
    deriving (Eq, Show, Storable)


type SubLinkRef src tgtType = LinkRef src (GetSublayout tgtType src)
