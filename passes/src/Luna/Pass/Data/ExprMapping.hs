module Luna.Pass.Data.ExprMapping where

import Luna.Prelude
import Luna.IR
import Data.Map as Map
import OCI.Pass.Manager
import Data.TypeDesc

newtype ExprMapping = ExprMapping (Map SomeExpr SomeExpr)
makeWrapped ''ExprMapping

initExprMapping :: MonadPassManager m => m ()
initExprMapping = setAttr (getTypeDesc @ExprMapping) $ ExprMapping Map.empty
