{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.Inference.TypeSimplification where

import qualified Data.List as List
import OCI.IR.Combinators
import Luna.IR
import OCI.Pass        (Pass, SubPass, Inputs, Outputs, Preserves, Events)
import Luna.Prelude as P hiding (cons)
import Luna.Pass.Inference.Data.SimplifierQueue
import Luna.Pass.Inference.Data.MergeQueue
import Luna.Pass.Inference.Data.Unifications
import qualified Luna.IR.Expr as Term
import Luna.IR.Expr (Term(Term))

data TypeSimplification
type instance Abstract TypeSimplification = TypeSimplification
type instance Inputs  Net   TypeSimplification = '[AnyExpr, AnyExprLink]
type instance Outputs Net   TypeSimplification = '[AnyExpr, AnyExprLink]
type instance Inputs  Layer TypeSimplification = '[AnyExpr // Model, AnyExpr // Succs, AnyExpr // Type, AnyExpr // Requester, AnyExpr // Errors, AnyExpr // RequiredBy, AnyExprLink // Model]
type instance Outputs Layer TypeSimplification = '[AnyExpr // Model, AnyExpr // Succs, AnyExpr // Type, AnyExpr // Requester, AnyExpr // Errors, AnyExpr // RequiredBy, AnyExprLink // Model]
type instance Inputs  Attr  TypeSimplification = '[SimplifierQueue, Unifications, MergeQueue]
type instance Outputs Attr  TypeSimplification = '[SimplifierQueue, Unifications, MergeQueue]
type instance Inputs  Event TypeSimplification = '[]
type instance Outputs Event TypeSimplification = '[New // AnyExpr, New // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink, OnDeepDelete // AnyExpr]
type instance Preserves     TypeSimplification = '[]

runTypeSimplification :: (MonadRef m, MonadIO m, MonadPassManager m) => SubPass TypeSimplification m Bool
runTypeSimplification = runTypeSimplification' False

runTypeSimplification' :: (MonadRef m, MonadIO m, MonadPassManager m) => Bool -> SubPass TypeSimplification m Bool
runTypeSimplification' nthRun = do
    apps <- unwrap <$> getAttr @SimplifierQueue
    res  <- mapM trySimplify apps
    putAttr @SimplifierQueue $ wrap' $ fmap snd $ filter (not . fst) $ zip res apps
    if or res then runTypeSimplification' True else return nthRun

destructMonad :: (MonadRef m, MonadIO m, MonadPassManager m) => Expr Draft -> SubPass TypeSimplification m (Expr Draft, Expr Draft)
destructMonad e = do
    Term (Term.Monadic t m) <- readTerm $ (unsafeGeneralize e :: Expr Monadic)
    (,) <$> source t <*> source m

trySimplify :: (MonadRef m, MonadIO m, MonadPassManager m) => Expr Monadic -> SubPass TypeSimplification m Bool
trySimplify tapp = do
    Term (Term.Monadic ap appMs) <- readTerm tapp
    onlyApp   <- source ap
    Term (Term.App     tf ta)    <- readTerm . (unsafeGeneralize :: Expr Draft -> Expr App) $ onlyApp
    appMonads <- source appMs
    tfun      <- source tf
    targ      <- source ta
    (funT, funM) <- destructMonad tfun
    (argT, argM) <- destructMonad targ
    matchExpr funT $ \case
        Lam ti to -> do
            tinp <- source ti
            tout <- source to
            tuni <- unify tinp targ
            requester <- getLayer @Requester tapp >>= mapM source
            reqBy     <- getLayer @RequiredBy tapp
            forM_ requester $ \r -> reconnectLayer' @Requester (Just r) tuni
            putLayer @RequiredBy tuni reqBy
            modifyAttr_ @Unifications $ wrap . (generalize tuni :) . unwrap
            (outT, outM) <- destructMonad tout
            jointM       <- unify funM outM
            modifyAttr_ @MergeQueue $ wrap . (generalize jointM :) . unwrap
            outInJoint <- monadic outT jointM
            replace jointM     appMonads
            modifyAttr_ @MergeQueue $ wrap . List.delete (unsafeGeneralize appMonads) . unwrap
            replace outT       onlyApp
            replace outInJoint tapp
            return True
        Cons n _ -> do
            requester <- getLayer @Requester tapp >>= mapM source
            reqBy     <- getLayer @RequiredBy tapp
            forM_ requester $ \r -> do
                modifyLayer_ @Errors r (CompileError ("Cannot use object of class " <> convert n <> " as a function") reqBy [] :)
            return True
        _ -> return False
