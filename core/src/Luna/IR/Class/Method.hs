module Luna.IR.Class.Method where

import Luna.Prelude
import Luna.IR
import Luna.IR.Function.Definition (CompiledFunction)
import Luna.IR.Name                (Name)

data Method = Method { _self :: AnyExpr
                     , _body :: CompiledFunction
                     }

makeLenses ''Method

