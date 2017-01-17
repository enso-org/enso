module Luna.IR.Module.Definition where

import           Luna.Prelude
import           Luna.IR
import           Luna.IR.Class.Definition    (Class)
import           Luna.IR.Function.Definition (CompiledFunction)
import           Luna.IR.Name                (Name)

import           Data.Map                    (Map)
import qualified Data.Map                    as Map

data Module = Module { _classes   :: Map Name Class
                     , _functions :: Map Name CompiledFunction
                     }

makeLenses ''Module

lookupFunction :: Module -> Name -> Maybe CompiledFunction
lookupFunction m n = Map.lookup n $ m ^. functions

lookupClass :: Module -> Name -> Maybe Class
lookupClass m n = Map.lookup n $ m ^. classes
