module Luna.IR.Component.Term.Discovery where

import Prologue

-- | Allows discovery of all registered terms when generating UniTerm.
class IsTermTag (t :: Type)
