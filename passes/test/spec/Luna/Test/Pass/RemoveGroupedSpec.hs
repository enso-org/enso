{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Luna.Test.Pass.RemoveGroupedSpec (spec) where

import           Control.Monad.Trans.Except
import qualified Control.Monad.State.Dependent.Old as Old
import           Control.Monad.Raise (tryAll)
import           OCI.Pass        (SubPass)
import qualified OCI.Pass        as Pass
import           Data.TypeDesc

import Test.Hspec   (Spec, Expectation, describe, it, shouldBe, shouldSatisfy)
import Luna.Prelude hiding (String, s, new)
import qualified Luna.Prelude as P
import qualified OCI.IR.Repr.Vis as Vis
import Luna.Test.Utils
import Luna.Test.IR.Runner
import Luna.IR
import Luna.Pass.Transform.Desugaring.RemoveGrouped
import System.Log
import Luna.Pass.Data.ExprRoots
import Control.Monad.State.Dependent
import Luna.IR.Analysis (checkIsoExpr)



noGroupedLeftBehind :: MonadPassManager m => SubPass RemoveGrouped m Bool
noGroupedLeftBehind = do
    es <- exprs
    or <$> forM es (flip matchExpr $ \case
        Grouped{} -> return True
        _         -> return False)

fooGrouped7 :: MonadPassManager m => SubPass RemoveGrouped m (Expr Draft)
fooGrouped7 = do
    foo <- var "foo"
    v <- var "x"
    a <- app foo v
    l <- lam v a
    g <- grouped l
    seven <- number 7
    generalize <$> app g seven

fooGrouped7Expected :: MonadPassManager m => SubPass RemoveGrouped m (Expr Draft)
fooGrouped7Expected = do
    foo <- var "foo"
    v <- var "x"
    a <- app foo v
    l <- lam v a
    seven <- number 7
    generalize <$> app l seven

manyApps :: MonadPassManager m => SubPass RemoveGrouped m (Expr Draft)
manyApps = do
    foo <- var "foo"
    seven <- number 7
    a1 <- app foo seven
    g1 <- grouped a1
    e <- number 2
    a2 <- app g1 e
    g2 <- grouped a2
    str <- string "bar"
    generalize <$> app g2 str

manyAppsExpected :: MonadPassManager m => SubPass RemoveGrouped m (Expr Draft)
manyAppsExpected = do
    foo <- var "foo"
    seven <- number 7
    a1 <- app foo seven
    e <- number 2
    a2 <- app a1 e
    str <- string "bar"
    generalize <$> app a2 str

accOnGrouped :: MonadPassManager m => SubPass RemoveGrouped m (Expr Draft)
accOnGrouped = do
    foo <- var "foo"
    x <- var "x"
    a <- app foo x
    g <- grouped a
    generalize <$> acc g "bar"

accOnGroupedExpected :: MonadPassManager m => SubPass RemoveGrouped m (Expr Draft)
accOnGroupedExpected = do
    foo <- var "foo"
    x <- var "x"
    a <- app foo x
    generalize <$> acc a "bar"

oneGrouped :: MonadPassManager m => SubPass RemoveGrouped m (Expr Draft)
oneGrouped = do
    one <- number 1
    generalize <$> grouped one

oneGroupedExpected :: MonadPassManager m => SubPass RemoveGrouped m (Expr Draft)
oneGroupedExpected = generalize <$> number 1

unifyExample :: MonadPassManager m => SubPass RemoveGrouped m (Expr Draft)
unifyExample = do
    n1  <- var "node1"
    two <- number 2
    ac  <- acc two "succ"
    g   <- grouped ac
    generalize <$> unify n1 g

unifyExpected :: MonadPassManager m => SubPass RemoveGrouped m (Expr Draft)
unifyExpected = do
    n1 <- var "node1"
    two <- number 2
    ac <- acc two "succ"
    generalize <$> unify n1 ac

fooOneGrouped :: MonadPassManager m => SubPass RemoveGrouped m (Expr Draft)
fooOneGrouped = do
    foo <- var "foo"
    one <- number 1 >>= grouped
    generalize <$> app foo one

fooOne :: MonadPassManager m => SubPass RemoveGrouped m (Expr Draft)
fooOne = do
    foo <- var "foo"
    one <- number 1
    generalize <$> app foo one

snapshotVis :: (MonadIR m, Vis.MonadVis m, MonadRef m) => P.String -> Pass.Pass TestPass m
snapshotVis = Vis.snapshot

desugarsTo :: Pass.KnownPass pass
           => SubPass pass (PassManager (IRBuilder (StateT Cache (Logger DropLogger (Old.StateT Vis.V Vis.Vis (ExceptT SomeException IO)))))) (Expr l)
           -> SubPass pass (PassManager (IRBuilder (StateT Cache (Logger DropLogger (Old.StateT Vis.V Vis.Vis (ExceptT SomeException IO)))))) (Expr l)
           -> Expectation
desugarsTo test expected = do
    Right (res, coherence, groups, orphans) <- tryAll $ silenceVis $ dropLogs $ evalDefStateT @Cache $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        setAttr (getTypeDesc @ExprRoots) $ ExprRoots []
        x <- Pass.eval' test
        desugared <- Pass.eval' $ flip evalStateT def $ removeGrouped $ generalize x
        void $ Pass.eval' $ snapshotVis "desugar"
        orphans   <- Pass.eval' @RemoveGrouped $ checkUnreachableExprs [desugared]
        coherence <- Pass.eval' @TestPass checkCoherence
        groups    <- Pass.eval' noGroupedLeftBehind
        expected' <- Pass.eval' expected
        result <- Pass.eval' $ checkIsoExpr @(SubPass RemoveGrouped _) (unsafeRelayout expected') (unsafeRelayout desugared)
        return (result, coherence, groups, orphans)
    res `shouldBe` True
    coherence `shouldSatisfy` null
    groups `shouldBe` False
    orphans `shouldSatisfy` null

spec :: Spec
spec = describe "remove grouped" $ do
    it "g(1) ==> 1" $ oneGrouped `desugarsTo` oneGroupedExpected
    it "\\x -> g(foo x) 7 ==> \\x -> foo x 7" $
        fooGrouped7 `desugarsTo` fooGrouped7Expected
    it "g(g(foo 7) 2) \"bar\" ==> foo 7 2 \"bar\"" $
        manyApps `desugarsTo` manyAppsExpected
    it "g(foo \"x\").bar ==> (foo \"x\").bar" $
        accOnGrouped `desugarsTo` accOnGroupedExpected
    it "node1 = g(2.succ) ==> node1 = 2.succ" $
        unifyExample `desugarsTo` unifyExpected
    it "foo g(1) ==> foo 1" $
        fooOneGrouped `desugarsTo` fooOne
