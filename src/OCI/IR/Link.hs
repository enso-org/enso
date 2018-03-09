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

-- data LinkData src tgt = LinkData
--     { _source :: {-# UNPACK #-} !(Term src)
--     , _target :: {-# UNPACK #-} !(Term tgt)
--     } deriving (Eq, Show)

data Source
data Target


data src :-: tgt

type instance Cmp Source Target = LT
type instance Cmp Target Source = GT
-- newtype Link src tgt = Link (Ptr (LinkData src tgt))
--     deriving (Eq, Show, Storable)

-- newtype Link (src :: Type) (tgt :: Type) = Link MData deriving (Eq, Show, Storable) -- FIXME: src not used
-- makeLenses ''Link
--
-- instance MutableData (Link src tgt) where
--     mdata = wrapped ; {-# INLINE mdata #-}


-- type SubLink src tgtType = Link (src :-: GetSublayout tgtType src)



-- === Instances === --

chunkSize' :: Int
chunkSize' = sizeOf' @Int ; {-# INLINE chunkSize' #-}

-- instance Storable (LinkData src tgt) where
--     sizeOf    _ = 2 * chunkSize' ; {-# INLINE sizeOf    #-}
--     alignment _ = chunkSize'     ; {-# INLINE alignment #-}
--     peek ptr = LinkData <$> peek (castPtr ptr) <*> peekByteOff ptr chunkSize'; {-# INLINE peek #-}
--     poke ptr (LinkData !a !b) = poke (castPtr ptr) a >> pokeByteOff ptr chunkSize' b
--     {-# INLINE poke #-}
