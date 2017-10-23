module Luna.Pass.Resolution.Data.CurrentTarget where

import Luna.IR
import Luna.Prelude
import OCI.Pass.Manager
import Data.TypeDesc

data CurrentTarget = TgtDef Name Name | TgtMethod Name Name Name | TgtNone deriving (Show, Eq)
makePrisms ''CurrentTarget

initCurrentTarget :: MonadPassManager m => m ()
initCurrentTarget = setAttr (getTypeDesc @CurrentTarget) TgtNone
