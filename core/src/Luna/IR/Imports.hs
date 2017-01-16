module Luna.IR.Imports where

import Luna.Prelude
import Luna.IR.Module.Definition
import Luna.IR.Name                (Name)
import           Data.Map          (Map)



newtype Imports = Imports (Map Name Module)
makeWrapped ''Imports
