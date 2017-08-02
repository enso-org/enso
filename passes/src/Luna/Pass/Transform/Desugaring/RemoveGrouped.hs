module Luna.Pass.Transform.Desugaring.RemoveGrouped where

import           OCI.Pass        (SubPass, Pass)
import qualified OCI.Pass        as Pass
import Luna.Prelude hiding (String, s, new)
import qualified Luna.Prelude as P
import qualified OCI.IR.Repr.Vis as Vis
import OCI.IR.Combinators
import Luna.IR
import Luna.Pass.Data.ExprRoots

import           Control.Monad.State.Dependent (evalStateT, StateT, get', modify'_)
import           Data.Set (Set)
import qualified Data.Set as Set

data RemoveGrouped
type instance Abstract   RemoveGrouped = RemoveGrouped
type instance Pass.Inputs     Net   RemoveGrouped = '[AnyExpr, AnyExprLink]
type instance Pass.Inputs     Layer RemoveGrouped = '[AnyExpr // Model, AnyExpr // Succs, AnyExpr // Type, AnyExprLink // Model]
type instance Pass.Inputs     Attr  RemoveGrouped = '[ExprRoots]
type instance Pass.Inputs     Event RemoveGrouped = '[]

type instance Pass.Outputs    Net   RemoveGrouped = '[AnyExpr, AnyExprLink]
type instance Pass.Outputs    Layer RemoveGrouped = '[AnyExpr // Model, AnyExpr // Type, AnyExprLink // Model, AnyExpr // Succs]
type instance Pass.Outputs    Attr  RemoveGrouped = '[ExprRoots]
type instance Pass.Outputs    Event RemoveGrouped = '[New // AnyExpr, Delete // AnyExpr, Delete // AnyExprLink, New // AnyExprLink, OnDeepDelete // AnyExpr]

type instance Pass.Preserves        RemoveGrouped = '[]

runRemoveGrouped :: (MonadRef m, MonadPassManager m) => Pass RemoveGrouped m
runRemoveGrouped = do
    roots    <- getAttr @ExprRoots
    newRoots <- flip evalStateT def $ forM (generalize <$> unwrap roots) removeGrouped
    putAttr @ExprRoots $ wrap $ unsafeGeneralize <$> newRoots

removeGrouped :: (MonadRef m, MonadPassManager m) => SomeExpr -> StateT (Set SomeExpr) (SubPass RemoveGrouped m) SomeExpr
removeGrouped e = do
    visited <- get'
    if Set.member e visited then return e else do
        modify'_ $ Set.insert e
        f <- inputs e
        mapM_ (removeGrouped <=< source) f
        matchExpr e $ \case
            Grouped g -> do
                g' <- source g
                substitute g' e
                deepDeleteWithWhitelist e $ Set.singleton g'
                return g'
            _ -> return e
