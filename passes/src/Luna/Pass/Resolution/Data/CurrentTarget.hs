module Luna.Pass.Resolution.Data.CurrentTarget where

import Luna.IR
import Luna.Prelude
import OCI.Pass.Manager
import Data.TypeDesc

data CurrentTarget = TgtDef Name | TgtMethod Name Name | TgtNone deriving (Show, Eq)

initCurrentTarget :: MonadPassManager m => m ()
initCurrentTarget = setAttr (getTypeDesc @CurrentTarget) TgtNone
