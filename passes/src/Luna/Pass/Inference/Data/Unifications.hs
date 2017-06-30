module Luna.Pass.Inference.Data.Unifications where

import Luna.IR
import Luna.Prelude
import OCI.Pass.Manager
import Data.TypeDesc

newtype Unifications = Unifications [Expr Unify]
makeWrapped ''Unifications

initUnifications :: MonadPassManager m => m ()
initUnifications = setAttr (getTypeDesc @Unifications) $ Unifications def
