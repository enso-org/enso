module Luna.IR.Term where

import Foreign.Ptr            (Ptr, castPtr)
import Foreign.Storable       (Storable, alignment, peek, peekByteOff, poke,
                               pokeByteOff, sizeOf)
import Foreign.Storable.Utils (alignment', castPtrTo, intPtr, sizeOf')
import Prologue

import qualified Data.Graph as Graph




import Luna.IR.Class
import OCI.IR.Term
-- import Luna.IR.Term.Basic as Basic


-- data VAR
-- data ACC
-- type Var = Term VAR
-- type Acc = Term ACC


-- type instance IRDef (Term a) =

-- data IR a
--     = Var !Int
--     | Acc !(IRLinkRef a) !(IRLinkRef a)
--     deriving (Generic, Show, Eq)
