module Luna.Pass.Resolution.Data.UnresolvedConses where

import Luna.IR
import Luna.Prelude
import OCI.Pass.Manager
import Data.TypeDesc

-- === Definition === --

newtype UnresolvedConses = UnresolvedConses [Expr Cons]
newtype NegativeConses   = NegativeConses [Expr Cons]
makeLenses ''UnresolvedConses
makeLenses ''NegativeConses


-- === Utils === --

initUnresolvedConses :: MonadPassManager m => m ()
initUnresolvedConses = setAttr (getTypeDesc @UnresolvedConses) $ UnresolvedConses []

initNegativeConses :: MonadPassManager m => m ()
initNegativeConses = setAttr (getTypeDesc @NegativeConses) $ NegativeConses []
