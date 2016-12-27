module Luna.IR.Class.Method where

import Luna.Prelude as P
import Luna.IR
import Luna.IR.Function.Definition (CompiledFunction)

data Method = Method { _self :: AnyExpr
                     , _body :: CompiledFunction
                     }

makeLenses ''Method

