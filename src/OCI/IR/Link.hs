module OCI.IR.Link where

import Prologue
import Foreign          (Ptr)
import Foreign.Storable (Storable)

import OCI.IR.Term
import OCI.IR.Layout hiding (Type)


import Foreign.Storable.Utils
import Foreign(castPtr)

type family IRDef a


------------------
-- === Link === --
------------------

-- === Definition === ---

data LinkData src tgt = LinkData
    { _source :: {-# UNPACK #-} !(IR src)
    , _target :: {-# UNPACK #-} !(IR tgt)
    } deriving (Eq, Show)

-- newtype Link src tgt = Link (Ptr (LinkData src tgt))
--     deriving (Eq, Show, Storable)

newtype Link (src :: Type) (tgt :: Type) = Link MData deriving (Eq, Show, Storable) -- FIXME: src not used
makeLenses ''Link

instance MutableData (Link src tgt) where
    mdata = wrapped ; {-# INLINE mdata #-}


-- type SubLink src tgtType = Link src (GetSublayout tgtType src)
type SubLink src tgtType = Link src (GetSublayout tgtType src)



-- === Instances === --

chunkSize' :: Int
chunkSize' = sizeOf' @Int ; {-# INLINE chunkSize' #-}

instance Storable (LinkData src tgt) where
    sizeOf    _ = 2 * chunkSize' ; {-# INLINE sizeOf    #-}
    alignment _ = chunkSize'     ; {-# INLINE alignment #-}
    peek ptr = LinkData <$> peek (castPtr ptr) <*> peekByteOff ptr chunkSize'; {-# INLINE peek #-}
    poke ptr (LinkData !a !b) = poke (castPtr ptr) a >> pokeByteOff ptr chunkSize' b
    {-# INLINE poke #-}
