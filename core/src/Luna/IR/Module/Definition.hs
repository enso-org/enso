module Luna.IR.Module.Definition where

import Luna.Prelude
import Luna.IR
import Luna.IR.Class.Definition    (Class)
import Luna.IR.Function.Definition (CompiledFunction)
import Data.Map                    (Map)
import Luna.IR.Name                (Name)

data Module = Module { _classes   :: Map Name Class
                     , _functions :: Map Name CompiledFunction
                     }

makeLenses ''Module
