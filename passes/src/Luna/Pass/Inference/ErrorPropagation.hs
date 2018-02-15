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
    matchExpr expr $ \case
        Seq a b   -> do
            propagateErrors =<< source a
            propagateErrors =<< source b
        -- FIXME[MM]: this line causes correct reporting of compile errors
        --            we don't want that until interpreter can cope with them
        --            and interpret the rest of the program
        -- ASGFunction _ _ g -> propagateErrors =<< source g
        _ -> do
            inpErrors <- fmap concat $ mapM (getErrors <=< source) =<< inputs expr
            modifyLayer_ @Errors expr $ nub . (++ inpErrors)

getErrors :: (MonadRef m, MonadPassManager m) => Expr Draft -> SubPass ErrorPropagation m [CompileError]
getErrors expr = do
    ownErrors    <- getLayer @Errors expr
    inputsErrors <- fmap concat $ mapM (getErrors <=< source) =<< inputs expr
    return (ownErrors ++ inputsErrors)
