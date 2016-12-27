module Luna.IR.Class.Definition where

import Luna.Prelude as P hiding (Constructor)
import Luna.IR
import Luna.IR.Class.Method        (Method)
import Luna.IR.Function.Definition (CompiledFunction)


data Constructor = Contructor { _body :: CompiledFunction
                              , _name :: P.String
                              }

makeLenses ''Constructor

data Class = Class { _constructors :: [Constructor]
                   , _methods      :: [Method]
                   }

makeLenses ''Class
