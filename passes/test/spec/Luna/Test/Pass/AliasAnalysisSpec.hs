{-# LANGUAGE OverloadedStrings #-}

module Luna.Test.Pass.AliasAnalysisSpec (spec) where

import           Control.Monad.Trans.Except
import qualified Control.Monad.State.Dependent.Old as Old
import           Control.Monad.Raise (tryAll)
import           OCI.Pass        (SubPass)
import qualified OCI.Pass        as Pass

import Test.Hspec   (Spec, Expectation, describe, it, shouldBe, shouldSatisfy)
import Luna.Prelude hiding (String, s, new, cons)
import qualified Luna.Prelude as P
import qualified OCI.IR.Repr.Vis as Vis
import Data.TypeDesc
import Luna.Test.Utils
import Luna.Pass.Resolution.AliasAnalysis
import Luna.Test.IR.Runner
import Luna.IR hiding (expr)
import System.Log
import Luna.Pass.Data.ExprRoots
import Luna.Pass.Resolution.Data.UnresolvedVars
import Luna.Pass.Resolution.Data.UnresolvedConses
import Control.Monad.State.Dependent
import Luna.IR.Analysis (checkIsoExpr)


snapshotVis :: (MonadIR m, Vis.MonadVis m, MonadRef m) => P.String -> Pass.Pass TestPass m
snapshotVis = Vis.snapshot

desugarsTo :: Pass.KnownPass pass
           => SubPass pass (PassManager (IRBuilder (StateT Cache (Logger DropLogger (Old.StateT Vis.V Vis.Vis (ExceptT SomeException IO)))))) [Expr Draft]
           -> SubPass pass (PassManager (IRBuilder (StateT Cache (Logger DropLogger (Old.StateT Vis.V Vis.Vis (ExceptT SomeException IO)))))) [Expr Draft]
           -> Expectation
desugarsTo test expected = do
    Right (res, coherence) <- tryAll $ silenceVis $ dropLogs $ evalDefStateT @Cache $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        initUnresolvedVars
        initUnresolvedConses
        initNegativeConses
        x <- Pass.eval' test
        setAttr (getTypeDesc @ExprRoots) $ ExprRoots x
        Pass.eval' runAliasAnalysis
        void $ Pass.eval' $ snapshotVis "desugar"
        coherence <- Pass.eval' @AliasAnalysis checkCoherence
        expected' <- Pass.eval' expected
        result <- Pass.eval' $ fmap and $
            zipWithM (checkIsoExpr @(SubPass AliasAnalysis _))
                     (map unsafeRelayout expected')
                     (map unsafeRelayout x)
        return (result, coherence)
    res `shouldBe` True
    coherence `shouldSatisfy` null

lamXFoo :: MonadPassManager m => SubPass TestPass m [Expr Draft]
lamXFoo = do
    x' <- var "x"
    ac <- acc x' "foo"
    x <- var "x"
    l <- lam x ac
    return [generalize l]

lamXFooExpected :: MonadPassManager m => SubPass TestPass m [Expr Draft]
lamXFooExpected = do
    x <- var "x"
    ac <- acc x "foo"
    l <- lam x ac
    return [generalize l]

idLam :: MonadPassManager m => SubPass TestPass m [Expr Draft]
idLam = do
    a <- var "a"
    a' <- var "a"
    l <- lam a a'
    return [generalize l]

idLamExpected :: MonadPassManager m => SubPass TestPass m [Expr Draft]
idLamExpected = do
    a <- var "a"
    l <- lam a a
    return [generalize l]

lamFooAB :: MonadPassManager m => SubPass TestPass m [Expr Draft]
lamFooAB = do
    a <- var "a"
    b <- var "b"
    foo <- var "foo"
    a1 <- app foo a
    a2 <- app a1 b
    a' <- var "a"
    b' <- var "b"
    l <- lam b' a2
    l1 <- lam a' l
    return [generalize l1]

lamFooABExpected :: MonadPassManager m => SubPass TestPass m [Expr Draft]
lamFooABExpected = do
    a <- var "a"
    b <- var "b"
    foo <- var "foo"
    a1 <- app foo a
    a2 <- app a1 b
    l <- lam b a2
    l1 <- lam a l
    return [generalize l1]

nEqFoo1BarE :: MonadPassManager m => SubPass TestPass m [Expr Draft]
nEqFoo1BarE = do
    u <- number 2
    {-foo <- var "foo"-}
    {-one <- number 1-}
    {-a1 <- app foo one-}
    {-bar <- string "bar"-}
    {-a2 <- app a1 bar-}
    {-e <- number 2-}
    {-a3 <- app a2 e-}
    {-n <- var "n"-}
    {-u <- unify n a3-}
    return [generalize u]

groupedFooAAppA :: MonadPassManager m => SubPass TestPass m [Expr Draft]
groupedFooAAppA = do
    one  <- number 1
    adef <- var "a"
    uni  <- unify adef one
    foo  <- var "foo"
    a    <- var "a"
    g    <- app foo a >>= grouped
    a'   <- var "a"
    a1   <- app g a'
    return [generalize uni, generalize a1]

groupedFooAAppAExpected :: MonadPassManager m => SubPass TestPass m [Expr Draft]
groupedFooAAppAExpected = do
    one  <- number 1
    a    <- var "a"
    uni  <- unify a one
    foo  <- var "foo"
    g    <- app foo a >>= grouped
    a1   <- app g a
    return [generalize uni, generalize a1]

allAbove :: MonadPassManager m => SubPass TestPass m [Expr Draft]
allAbove = do
    (t1 :: [Expr Draft]) <- map generalize <$> lamXFoo
    (t2 :: [Expr Draft]) <- map generalize <$> idLam
    (t3 :: [Expr Draft]) <- map generalize <$> lamFooAB
    (t4 :: [Expr Draft]) <- map generalize <$> nEqFoo1BarE
    (t5 :: [Expr Draft]) <- map generalize <$> groupedFooAAppA
    return $ concat [t1, t2, t3, t4, t5]

allAboveExpected :: MonadPassManager m => SubPass TestPass m [Expr Draft]
allAboveExpected = do
    (t1 :: [Expr Draft]) <- map generalize <$> lamXFooExpected
    (t2 :: [Expr Draft]) <- map generalize <$> idLamExpected
    (t3 :: [Expr Draft]) <- map generalize <$> lamFooABExpected
    (t4 :: [Expr Draft]) <- map generalize <$> nEqFoo1BarE
    (t5 :: [Expr Draft]) <- map generalize <$> groupedFooAAppAExpected
    return $ concat [t1, t2, t3, t4, t5]

manyApps :: MonadPassManager m => SubPass TestPass m [Expr Draft]
manyApps = do
    u1 <- generalize <$> do
        foo <- var "foo"
        a <- var "a"
        n1 <- var "n1"
        unify n1 =<< app foo a
    u2 <- generalize <$> do
        bar <- var "bar"
        a <- var "a"
        b <- var "b"
        ap1 <- app bar a
        n2 <- var "n2"
        unify n2 =<< app ap1 b
    u3 <- generalize <$> do
        baz <- var "baz"
        a <- var "a"
        b <- var "b"
        c <- var "c"
        ap1 <- app baz a
        ap2 <- app ap1 b
        ap3 <- app ap2 c
        n3 <- var "n3"
        unify n3 ap3
    return [u1, u2, u3]

manyAppsExpected :: MonadPassManager m => SubPass TestPass m [Expr Draft]
manyAppsExpected = do
    a <- var "a"
    b <- var "b"
    u1 <- generalize <$> do
        foo <- var "foo"
        n1 <- var "n1"
        unify n1 =<< app foo a
    u2 <- generalize <$> do
        bar <- var "bar"
        ap1 <- app bar a
        n2 <- var "n2"
        unify n2 =<< app ap1 b
    u3 <- generalize <$> do
        baz <- var "baz"
        c <- var "c"
        ap1 <- app baz a
        ap2 <- app ap1 b
        ap3 <- app ap2 c
        n3 <- var "n3"
        unify n3 ap3
    return [u1, u2, u3]

lamPattern :: MonadPassManager m => SubPass TestPass m [Expr Draft]
lamPattern = do
    a <- var "a"
    b <- var "b"
    c <- var "c"
    pat <- cons "Tuple3" [a,b,c]
    b' <- var "b"
    l <- lam pat b'
    return [generalize l]

lamPatternExpected :: MonadPassManager m => SubPass TestPass m [Expr Draft]
lamPatternExpected = do
    a <- var "a"
    b <- var "b"
    c <- var "c"
    pat <- cons "Tuple3" [a,b,c]
    l <- lam pat b
    return [generalize l]

simpleLamPattern :: MonadPassManager m => SubPass TestPass m [Expr Draft]
simpleLamPattern = do
    a <- var "a"
    pat <- cons "Box" [a]
    a' <- var "a"
    l <- lam pat a'
    return [generalize l]

simpleLamPatternExpected :: MonadPassManager m => SubPass TestPass m [Expr Draft]
simpleLamPatternExpected = do
    a <- var "a"
    pat <- cons "Box" [a]
    l <- lam pat a
    return [generalize l]

spec :: Spec
spec = describe "gather vars" $ do
    it "\\x -> x.foo" $ lamXFoo `desugarsTo` lamXFooExpected
    it "\\a -> a" $ idLam `desugarsTo` idLamExpected
    it "\\a b -> foo a b" $ lamFooAB `desugarsTo` lamFooABExpected
    it "n = foo 1 \"bar\" 2.718" $ nEqFoo1BarE `desugarsTo` nEqFoo1BarE
    it "a = 1; (foo a) a" $ groupedFooAAppA `desugarsTo` groupedFooAAppAExpected
    it "all of the above" $ allAbove `desugarsTo` allAboveExpected
    it "n1 = foo a; n2 = bar a b; n3 = baz a b c" $ manyApps `desugarsTo` manyAppsExpected
    it "\\(Box a) -> a" $ simpleLamPattern `desugarsTo` simpleLamPatternExpected
    it "\\(Tuple3 a b c) -> b" $ lamPattern `desugarsTo` lamPatternExpected
