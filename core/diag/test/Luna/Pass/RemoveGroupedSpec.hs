{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -Wno-partial-type-signatures #-}

module Luna.Pass.RemoveGroupedSpec (spec) where

import           Control.Monad.Raise (tryAll)
import           Luna.Pass        (SubPass)
import qualified Luna.Pass        as Pass

import Test.Hspec   (Spec, Expectation, describe, it, shouldBe, shouldSatisfy)
import Luna.Prelude hiding (String, s, new)
import qualified Luna.Prelude as P
import qualified Luna.IR.Repr.Vis as Vis
import Luna.TestUtils
import Luna.IR.Expr.Combinators
import Luna.IR.Function hiding (args)
import Luna.IR.Runner
import Luna.IR
import System.Log


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

removeGrouped :: _ => SomeExpr -> SubPass RemoveGrouped _ SomeExpr
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

noGroupedLeftBehind :: _ => SubPass RemoveGrouped m Bool
noGroupedLeftBehind = do
    es <- exprs
    or <$> forM es (flip match $ \case
        Grouped{} -> return True
        _         -> return False)

fooGrouped7 :: _ => SubPass RemoveGrouped _ _
fooGrouped7 = do
    foo <- strVar "foo"
    v <- strVar "x"
    a <- app foo (arg v)
    l <- lam (arg v) a
    g <- grouped l
    seven <- integer (7::Int)
    app g (arg seven)

fooGrouped7Expected :: _ => SubPass RemoveGrouped _ _
fooGrouped7Expected = do
    foo <- strVar "foo"
    v <- strVar "x"
    a <- app foo (arg v)
    l <- lam (arg v) a
    seven <- integer (7::Int)
    app l (arg seven)

manyApps :: _ => SubPass RemoveGrouped _ _
manyApps = do
    foo <- strVar "foo"
    seven <- integer (7::Int)
    a1 <- app foo (arg seven)
    g1 <- grouped a1
    e <- rational 2.718
    a2 <- app g1 (arg e)
    g2 <- grouped a2
    str <- string "bar"
    app g2 (arg str)

manyAppsExpected :: _ => SubPass RemoveGrouped _ _
manyAppsExpected = do
    foo <- strVar "foo"
    seven <- integer (7::Int)
    a1 <- app foo (arg seven)
    e <- rational 2.718
    a2 <- app a1 (arg e)
    str <- string "bar"
    app a2 (arg str)

accOnGrouped :: _ => SubPass RemoveGrouped _ _
accOnGrouped = do
    foo <- strVar "foo"
    x <- strVar "x"
    a <- app foo (arg x)
    g <- grouped a
    rawAcc "bar" g

accOnGroupedExpected :: _ => SubPass RemoveGrouped _ _
accOnGroupedExpected = do
    foo <- strVar "foo"
    x <- strVar "x"
    a <- app foo (arg x)
    rawAcc "bar" a

oneGrouped :: _ => SubPass RemoveGrouped _ _
oneGrouped = do
    one <- integer (1::Int)
    grouped one

oneGroupedExpected :: _ => SubPass RemoveGrouped _ _
oneGroupedExpected = integer (1::Int)

unifyExample :: _ => SubPass RemoveGrouped _ _
unifyExample = do
    n1 <- strVar "node1"
    two <- integer (2::Int)
    ac <- rawAcc "succ" two
    g <- grouped ac
    unify n1 g

unifyExpected :: _ => SubPass RemoveGrouped _ _
unifyExpected = do
    n1 <- strVar "node1"
    two <- integer (2::Int)
    ac <- rawAcc "succ" two
    unify n1 ac

fooOneGrouped :: _ => SubPass RemoveGrouped _ _
fooOneGrouped = do
    foo <- strVar "foo"
    one <- integer (1::Int) >>= grouped
    app foo (arg one)

fooOne :: _ => SubPass RemoveGrouped _ _
fooOne = do
    foo <- strVar "foo"
    one <- integer (1::Int)
    app foo (arg one)

snapshotVis :: (MonadIR m, Vis.MonadVis m, MonadRef m) => P.String -> Pass.Pass TestPass m
snapshotVis = Vis.snapshot

desugarsTo :: _ => _ -> _ -> Expectation
desugarsTo test expected = do
    Right (res, coherence, groups, orphans) <- tryAll $ withVis $ dropLogs $ runRefCache $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        x <- Pass.eval' test
        desugared <- Pass.eval' $ removeGrouped $ generalize x
        void $ Pass.eval' $ snapshotVis "desugar"
        orphans   <- Pass.eval' @RemoveGrouped $ checkUnreachableExprs [desugared]
        coherence <- Pass.eval' @RemoveGrouped checkCoherence
        groups    <- Pass.eval' noGroupedLeftBehind
        expected' <- Pass.eval' expected
        result <- Pass.eval' $ areExpressionsIsomorphic @(SubPass RemoveGrouped _) (unsafeRelayout expected') (unsafeRelayout desugared)
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
    it "g(g(foo 7) 2.718) \"bar\" ==> foo 7 2.718 \"bar\"" $
        manyApps `desugarsTo` manyAppsExpected
    it "g(foo \"x\").bar ==> (foo \"x\").bar" $
        accOnGrouped `desugarsTo` accOnGroupedExpected
    it "node1 = g(2.succ) ==> node1 = 2.succ" $
        unifyExample `desugarsTo` unifyExpected
    it "foo g(1) ==> foo 1" $
        fooOneGrouped `desugarsTo` fooOne
