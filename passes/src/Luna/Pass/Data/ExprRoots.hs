module Luna.Pass.Data.ExprRoots where

import Luna.Prelude
import Luna.IR

newtype ExprRoots = ExprRoots [Expr Draft]
makeWrapped ''ExprRoots
