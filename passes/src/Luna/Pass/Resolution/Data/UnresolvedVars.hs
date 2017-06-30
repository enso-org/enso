module Luna.Pass.Resolution.Data.UnresolvedVars where

import Luna.IR
import Luna.Prelude
import OCI.Pass.Manager
import Data.TypeDesc


newtype UnresolvedVars = UnresolvedVars [Expr Var]
makeWrapped ''UnresolvedVars

initUnresolvedVars :: MonadPassManager m => m ()
initUnresolvedVars = setAttr (getTypeDesc @UnresolvedVars) $ UnresolvedVars []
