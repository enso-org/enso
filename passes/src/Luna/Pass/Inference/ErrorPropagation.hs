module Luna.Pass.Inference.ErrorPropagation where

import OCI.IR.Combinators
import Luna.IR
import OCI.Pass        (Pass, SubPass, Inputs, Outputs, Preserves, Events)
import Luna.Prelude as P hiding (cons)
import qualified Luna.IR.Expr as Term
import Luna.IR.Expr (Term(Term))
import Luna.Pass.Data.ExprRoots
import Data.List (nub)

data ErrorPropagation
type instance Abstract ErrorPropagation = ErrorPropagation
type instance Inputs  Net   ErrorPropagation = '[AnyExpr, AnyExprLink]
type instance Outputs Net   ErrorPropagation = '[AnyExpr, AnyExprLink]
type instance Inputs  Layer ErrorPropagation = '[AnyExpr // Model, AnyExpr // Succs, AnyExprLink // Model, AnyExpr // Errors]
type instance Outputs Layer ErrorPropagation = '[AnyExpr // Model, AnyExpr // Succs, AnyExprLink // Model, AnyExpr // Errors]
type instance Inputs  Attr  ErrorPropagation = '[ExprRoots]
type instance Outputs Attr  ErrorPropagation = '[]
type instance Inputs  Event ErrorPropagation = '[]
type instance Outputs Event ErrorPropagation = '[]
type instance Preserves     ErrorPropagation = '[]

run :: (MonadRef m, MonadPassManager m) => Pass ErrorPropagation m
run = do
    roots <- unwrap <$> getAttr @ExprRoots
    mapM_ propagateErrors roots

propagateErrors :: (MonadRef m, MonadPassManager m) => Expr Draft -> SubPass ErrorPropagation m ()
propagateErrors expr = do
    let updateErrors e = do
            inpErrors <- fmap concat $ mapM (getErrors <=< source) =<< inputs e
            modifyLayer_ @Errors expr $ nub . (++ inpErrors)
    matchExpr expr $ \case
        Seq a b   -> do
            propagateErrors =<< source a
            propagateErrors =<< source b
        ASGFunction _ _ g -> do
            propagateErrors =<< source g
            updateErrors expr
        Marked _ e -> do
            propagateErrors =<< source e
            updateErrors expr
        Unify a b -> do
            propagateErrors =<< source a
            propagateErrors =<< source b
            updateErrors expr
            unifyErrors <- getLayer @Errors expr
            (flip (modifyLayer_ @Errors)) (nub . (++ unifyErrors)) =<< source a
        _ -> do
            updateErrors expr

getErrors :: (MonadRef m, MonadPassManager m) => Expr Draft -> SubPass ErrorPropagation m [CompileError]
getErrors expr = do
    ownErrors    <- getLayer @Errors expr
    inputsErrors <- fmap concat $ mapM (getErrors <=< source) =<< inputs expr
    return (ownErrors ++ inputsErrors)
