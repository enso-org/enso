module Luna.IR.Class.Definition where

import Luna.Prelude                hiding (Constructor)
import Luna.IR
import Luna.IR.Class.Method        (Method)
import Luna.IR.Function.Definition (CompiledFunction)
import Luna.IR.Name                (Name)
import Data.Map                    (Map)


newtype Constructor = Contructor { _body :: CompiledFunction }

makeLenses ''Constructor

data Class = Class { _constructors :: Map Name Constructor
                   , _methods      :: Map Name Method
                   }

makeLenses ''Class
