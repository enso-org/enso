module Luna.Pass.Desugaring.RemoveGrouped where

import           Luna.Pass        (SubPass)
import qualified Luna.Pass        as Pass
import Luna.Prelude hiding (String, s, new)
import qualified Luna.Prelude as P
import qualified Luna.IR.Repr.Vis as Vis
import Luna.IR.Expr.Combinators
import Luna.IR


data RemoveGrouped
type instance Abstract   RemoveGrouped = RemoveGrouped
type instance Pass.Inputs     Net   RemoveGrouped = '[AnyExpr, AnyExprLink]
type instance Pass.Inputs     Layer RemoveGrouped = '[AnyExpr // Model, AnyExpr // Succs, AnyExpr // Type, AnyExprLink // Model]
type instance Pass.Inputs     Attr  RemoveGrouped = '[]
type instance Pass.Inputs     Event RemoveGrouped = '[]

type instance Pass.Outputs    Net   RemoveGrouped = '[AnyExpr, AnyExprLink]
type instance Pass.Outputs    Layer RemoveGrouped = '[AnyExpr // Model, AnyExpr // Type, AnyExprLink // Model, AnyExpr // Succs]
type instance Pass.Outputs    Attr  RemoveGrouped = '[]
type instance Pass.Outputs    Event RemoveGrouped = '[New // AnyExpr, Delete // AnyExpr, Delete // AnyExprLink, New // AnyExprLink]

type instance Pass.Preserves        RemoveGrouped = '[]

removeGrouped :: (MonadRef m, MonadPassManager m) => SomeExpr -> SubPass RemoveGrouped m SomeExpr
removeGrouped e = do
    f <- symbolFields e
    mapM_ (removeGrouped <=< source) f
    match e $ \case
        Grouped g -> do
            g' <- source g
            replaceNode e g'
            deleteWithoutInputs e
            return g'
        _ -> return e
