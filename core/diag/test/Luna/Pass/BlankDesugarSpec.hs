{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wall -Wno-partial-type-signatures #-}

module Luna.Pass.BlankDesugarSpec (spec) where

import           Luna.Pass        (SubPass)
import qualified Luna.Pass        as Pass

import Luna.Pass.Desugaring.BlankArguments

import Test.Hspec   (Spec, Expectation, describe, it, shouldBe, shouldSatisfy)
import Luna.Prelude hiding (String, s, new)
import qualified Luna.Prelude as P
import Data.TypeDesc
import qualified Luna.IR.Repr.Vis as Vis
import Luna.TestUtils
import Luna.IR.Function hiding (args)
import Luna.IR.Runner
import Luna.IR
import System.Log



blankDotFoo :: _ => SubPass BlankDesugaring _ _
blankDotFoo = do
    b <- blank
    rawAcc "foo" b

blankDotFooExpected :: _ => SubPass BlankDesugaring _ _
blankDotFooExpected = do
    v <- strVar obscureName
    a <- rawAcc "foo" v
    lam (arg v) a

snapshotVis :: (MonadIR m, Vis.MonadVis m, MonadRef m) => P.String -> Pass.Pass TestPass m
snapshotVis = Vis.snapshot

noBlankLeftBehind :: _ => SubPass BlankDesugaring m Bool
noBlankLeftBehind = do
    es <- exprs
    or <$> forM es (flip match $ \case
        Blank -> return True
        _     -> return False)

desugarsTo :: _ => _ -> _ -> Expectation
desugarsTo test expected = do
    (res, coherence, blanks, orphans) <- withVis $ dropLogs $ runRefCache $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        setAttr (getTypeDesc @UniqueNameGen) $ UniqueNameGen ("obscureName", (0::Int))
        setAttr (getTypeDesc @UsedVars) $ UsedVars []
        Right x <- Pass.eval' test
        Right desugared <- Pass.eval' $ desugar $ generalize x
        void $ Pass.eval' $ snapshotVis "desugar"
        Right orphans   <- Pass.eval' @BlankDesugaring $ checkUnreachableExprs [desugared]
        Right coherence <- Pass.eval' @BlankDesugaring checkCoherence
        Right blanks    <- Pass.eval' noBlankLeftBehind
        Right expected' <- Pass.eval' expected
        Right result <- Pass.eval' $ areExpressionsIsomorphic @(SubPass BlankDesugaring _) (unsafeRelayout expected') (unsafeRelayout desugared)
        return (result, coherence, blanks, orphans)
    res `shouldBe` True
    coherence `shouldSatisfy` null
    blanks `shouldBe` False
    orphans `shouldSatisfy` null

replacesTo :: _ => _ -> _ -> Expectation
replacesTo test expected = do
    res <- dropLogs $ runRefCache $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        setAttr (getTypeDesc @UniqueNameGen) $ UniqueNameGen ("obscureName", (0::Int))
        setAttr (getTypeDesc @UsedVars) $ UsedVars []
        Right x <- Pass.eval' test
        Right desugared <- Pass.eval' $ replaceBlanks $ generalize x
        Right expected' <- Pass.eval' $ expected
        Right result <- Pass.eval' $ areExpressionsIsomorphic @(SubPass TestPass _) (unsafeRelayout expected') (unsafeRelayout desugared)
        return result
    res `shouldBe` True

fooBlank7 :: _ => SubPass BlankDesugaring _ _
fooBlank7 = do
    foo <- strVar "foo"
    b <- blank
    seven <- integer (7::Int)
    app1 <- app foo (arg b)
    app app1 (arg seven)

fooBlank7Replaced :: _ => SubPass BlankDesugaring _ _
fooBlank7Replaced = do
    foo <- strVar "foo"
    v <- strVar obscureName
    seven <- integer (7::Int)
    app1 <- app foo (arg v)
    app app1 (arg seven)

fooBlank7Expected :: _ => SubPass BlankDesugaring _ _
fooBlank7Expected = do
    foo <- strVar "foo"
    v <- strVar obscureName
    seven <- integer (7::Int)
    app1 <- app foo (arg v)
    app2 <- app app1 (arg seven)
    lam (arg v) app2

fooBlank :: _ => SubPass BlankDesugaring _ _
fooBlank = do
    foo <- strVar "foo"
    b <- blank
    app foo (arg b)

fooBlankReplaced :: _ => SubPass BlankDesugaring _ _
fooBlankReplaced = do
    foo <- strVar "foo"
    v <- strVar obscureName
    app foo (arg v)

fooBlankExpected :: _ => SubPass BlankDesugaring _ _
fooBlankExpected = do
    foo <- strVar "foo"
    v <- strVar obscureName
    a <- app foo (arg v)
    lam (arg v) a

fooBlankGrouped7 :: _ => SubPass BlankDesugaring _ _
fooBlankGrouped7 = do
    foo <- strVar "foo"
    b <- blank
    a <- app foo (arg b)
    g <- grouped a
    seven <- integer (7::Int)
    app g (arg seven)

fooBlankGrouped7Expected :: _ => SubPass BlankDesugaring _ _
fooBlankGrouped7Expected = do
    foo <- strVar "foo"
    v <- strVar obscureName
    a <- app foo (arg v)
    l <- lam (arg v) a
    g <- grouped l
    seven <- integer (7::Int)
    app g (arg seven)

blankDotFooBlankBar :: _ => SubPass BlankDesugaring _ _
blankDotFooBlankBar = do
    bDotFoo <- blankDotFoo
    b <- blank
    bar <- strVar "bar"
    a <- app b (arg bar)
    app bDotFoo (arg a)

blankDotFooBlankBarReplaced :: _ => SubPass BlankDesugaring _ _
blankDotFooBlankBarReplaced = do
    v0 <- strVar obscureName
    vDotFoo <- rawAcc "foo" v0
    v1 <- strVar "^obscureName1"
    bar <- strVar "bar"
    a <- app v1 (arg bar)
    app vDotFoo (arg a)

blankDotFooBlankBarExpected :: _ => SubPass BlankDesugaring _ _
blankDotFooBlankBarExpected = do
    v0 <- strVar obscureName
    v1 <- strVar "^obscureName1"
    bar <- strVar "bar"
    a <- app v1 (arg bar)
    ac <- rawAcc "foo" v0
    a' <- app ac (arg a)
    l <- lam (arg v1) a'
    lam (arg v0) l


spec :: Spec
spec = do
  describe "replace blanks" $ do
    it "foo _ => foo ^obscureName0" $ fooBlank `replacesTo` fooBlankReplaced
    it "foo _ 7 => foo ^obscureName0 7" $ fooBlank7 `replacesTo` fooBlank7Replaced
    it "_.foo _ bar => ^obscureName0.foo ^obscureName1 bar" $ blankDotFooBlankBar `replacesTo` blankDotFooBlankBarReplaced
  describe "blank desugaring" $ do
    it "_.foo => (\\x -> x.foo)" $
      blankDotFoo `desugarsTo` blankDotFooExpected
    it "_ 1 => (\\x -> x 1)" $ do
      let blank1 :: _ => SubPass BlankDesugaring _ _
          blank1 = do
            b <- blank
            one <- integer (1::Int)
            app b (arg one)

          blank1Expected :: _ => SubPass BlankDesugaring _ _
          blank1Expected = do
            v <- strVar obscureName
            one <- integer (1::Int)
            a <- app v (arg one)
            lam (arg v) a
      blank1 `desugarsTo` blank1Expected
    it "foo _ => (\\x -> foo x)" $
      fooBlank `desugarsTo` fooBlankExpected
    it "foo _ 7 => (\\x -> foo x 7)" $
      fooBlank7 `desugarsTo` fooBlank7Expected
    it "(foo _) 7 => (\\x -> foo x) 7" $
      fooBlankGrouped7 `desugarsTo` fooBlankGrouped7Expected
    it "_.foo _ bar => (\\x y -> x.foo y bar)" $
      blankDotFooBlankBar `desugarsTo` blankDotFooBlankBarExpected
    it "(_.foo) _ bar => (\\y -> (\\x -> x.foo) y bar)" $ do
      let blankDotFooGroupedBlankBar :: _ => SubPass BlankDesugaring _ _
          blankDotFooGroupedBlankBar = do
            bDotFoo <- blankDotFoo
            b <- blank
            bar <- strVar "bar"
            a <- app b (arg bar)
            g <- grouped bDotFoo
            app g (arg a)

          blankDotFooGroupedBlankBarExpected :: _ => SubPass BlankDesugaring _ _
          blankDotFooGroupedBlankBarExpected = do
            v0 <- strVar obscureName
            v1 <- strVar "^obscureName1"
            bar <- strVar "bar"
            a <- app v1 (arg bar)
            ac <- rawAcc "foo" v0
            l <- lam (arg v0) ac
            g <- grouped l
            a' <- app g (arg a)
            lam (arg v1) a'
      blankDotFooGroupedBlankBar `desugarsTo` blankDotFooGroupedBlankBarExpected
    it "foo bar _ baz => (\\x -> foo bar x baz)" $ do
      let fooBarBlankBaz :: _ => SubPass BlankDesugaring _ _
          fooBarBlankBaz = do
            foo <- strVar "foo"
            bar <- strVar "bar"
            baz <- strVar "baz"
            b <- blank
            a <- app b (arg baz)
            a' <- app bar (arg a)
            app foo (arg a')

          fooBarBlankBazExpected :: _ => SubPass BlankDesugaring _ _
          fooBarBlankBazExpected = do
            v0 <- strVar obscureName
            foo <- strVar "foo"
            bar <- strVar "bar"
            baz <- strVar "baz"
            a <- app v0 (arg baz)
            a' <- app bar (arg a)
            a'' <- app foo (arg a')
            lam (arg v0) a''
      fooBarBlankBaz `desugarsTo` fooBarBlankBazExpected
    it "\\y -> _.foo y => (\\y -> \\x -> x.foo y)" $ do
      let lamYBlankFooY :: _ => SubPass BlankDesugaring _ _
          lamYBlankFooY = do
            y <- strVar "y"
            b <- blank
            foo <- rawAcc "foo" b
            a <- app foo (arg y)
            lam (arg y) a

          lamYBlankFooYExpected :: _ => SubPass BlankDesugaring _ _
          lamYBlankFooYExpected = do
            y <- strVar "y"
            v <- strVar obscureName
            foo <- rawAcc "foo" v
            a <- app foo (arg y)
            l <- lam (arg v) a
            lam (arg y) l
      lamYBlankFooY `desugarsTo` lamYBlankFooYExpected
