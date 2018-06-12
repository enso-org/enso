module Luna.Pass.Resolve.Data.UnresolvedVariables where

import Prologue

import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr

newtype UnresolvedVariables = UnresolvedVariables [IR.Term IR.Var]
type instance Attr.Type UnresolvedVariables = Attr.Atomic
instance Default UnresolvedVariables where
    def = UnresolvedVariables def

makeLenses ''UnresolvedVariables
