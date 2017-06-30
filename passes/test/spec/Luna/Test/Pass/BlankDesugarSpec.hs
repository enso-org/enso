{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wall #-}

module Luna.Test.Pass.BlankDesugarSpec (spec) where

import           Control.Monad.Trans.Except
import qualified Control.Monad.State.Dependent.Old as Old
import           Control.Monad.Raise (tryAll)
import           OCI.Pass        (SubPass)
import qualified OCI.Pass        as Pass

import Luna.Pass.Transform.Desugaring.BlankArguments

import Test.Hspec   (Spec, Expectation, describe, it, shouldBe, shouldSatisfy)
import Luna.Prelude hiding (String, s, new)
import qualified Luna.Prelude as P
import Data.TypeDesc
import qualified OCI.IR.Repr.Vis as Vis
import Luna.Test.Utils
import Luna.Test.IR.Runner
import Luna.Pass.Data.UniqueNameGen
import Luna.IR
import System.Log
import Luna.Pass.Data.ExprRoots
import Control.Monad.State.Dependent
import Luna.IR.Analysis (checkIsoExpr)


obscureName :: Name
obscureName = "#a"

blankDotFoo :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
blankDotFoo = do
    b <- blank
    generalize <$> acc b "foo"

blankDotFooExpected :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
blankDotFooExpected = do
    v <- var obscureName
    a <- acc v "foo"
    generalize <$> lam v a

snapshotVis :: (MonadIR m, Vis.MonadVis m, MonadRef m) => P.String -> Pass.Pass TestPass m
snapshotVis = Vis.snapshot

noBlankLeftBehind :: MonadPassManager m => SubPass BlankDesugaring m Bool
noBlankLeftBehind = do
    es <- exprs
    or <$> forM es (flip matchExpr $ \case
        Blank -> return True
        _     -> return False)

desugarsTo :: Pass.KnownPass pass
           => SubPass pass (PassManager (IRBuilder (StateT Cache (Logger DropLogger (Old.StateT Vis.V Vis.Vis (ExceptT SomeException IO)))))) (Expr Draft)
           -> SubPass pass (PassManager (IRBuilder (StateT Cache (Logger DropLogger (Old.StateT Vis.V Vis.Vis (ExceptT SomeException IO)))))) (Expr Draft)
           -> Expectation
desugarsTo test expected = do
    Right (res, coherence, blanks, orphans) <- tryAll $ silenceVis $ dropLogs $ evalDefStateT @Cache $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        initNameGen
        setAttr (getTypeDesc @ExprRoots) $ ExprRoots []
        x <- Pass.eval' test
        desugared <- Pass.eval' $ desugar $ generalize x
        void $ Pass.eval' $ snapshotVis "desugar"
        orphans   <- Pass.eval' @BlankDesugaring $ checkUnreachableExprs [generalize desugared]
        coherence <- Pass.eval' @BlankDesugaring checkCoherence
        blanks    <- Pass.eval' noBlankLeftBehind
        expected' <- Pass.eval' expected
        result <- Pass.eval' $ checkIsoExpr @(SubPass BlankDesugaring _) (unsafeRelayout expected') (unsafeRelayout desugared)
        return (result, coherence, blanks, orphans)
    res `shouldBe` True
    coherence `shouldSatisfy` null
    blanks `shouldBe` False
    orphans `shouldSatisfy` null

fooBlank7 :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
fooBlank7 = do
    foo <- var "foo"
    b <- blank
    seven <- number 7
    app1 <- app foo b
    generalize <$> app app1 seven

fooBlank7Replaced :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
fooBlank7Replaced = do
    foo <- var "foo"
    v <- var obscureName
    seven <- number 7
    app1 <- app foo v
    generalize <$> app app1 seven

fooBlank7Expected :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
fooBlank7Expected = do
    foo <- var "foo"
    v <- var obscureName
    seven <- number 7
    app1 <- app foo v
    app2 <- app app1 seven
    generalize <$> lam v app2

fooBlank :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
fooBlank = do
    foo <- var "foo"
    b <- blank
    generalize <$> app foo b

fooBlankReplaced :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
fooBlankReplaced = do
    foo <- var "foo"
    v <- var obscureName
    generalize <$> app foo v

fooBlankExpected :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
fooBlankExpected = do
    foo <- var "foo"
    v <- var obscureName
    a <- app foo v
    generalize <$> lam v a

fooBlankGrouped7 :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
fooBlankGrouped7 = do
    foo <- var "foo"
    b <- blank
    a <- app foo b
    g <- grouped a
    seven <- number 7
    generalize <$> app g seven

fooBlankGrouped7Expected :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
fooBlankGrouped7Expected = do
    foo <- var "foo"
    v <- var obscureName
    a <- app foo v
    l <- lam v a
    g <- grouped l
    seven <- number 7
    generalize <$> app g seven

blankDotFooBlankBar :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
blankDotFooBlankBar = do
    bDotFoo <- blankDotFoo
    b <- blank
    bar <- var "bar"
    a <- app b bar
    generalize <$> app bDotFoo a

blankDotFooBlankBarReplaced :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
blankDotFooBlankBarReplaced = do
    v0 <- var obscureName
    vDotFoo <- acc v0 "foo"
    v1 <- var "#b"
    bar <- var "bar"
    a <- app v1 bar
    generalize <$> app vDotFoo a

blankDotFooBlankBarExpected :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
blankDotFooBlankBarExpected = do
    v0 <- var obscureName
    v1 <- var "#b"
    bar <- var "bar"
    a <- app v1 bar
    ac <- acc v0 "foo"
    a' <- app ac a
    l <- lam v1 a'
    generalize <$> lam v0 l


spec :: Spec
spec = do
  describe "blank desugaring" $ do
    it "_.foo => (\\x -> x.foo)" $
      blankDotFoo `desugarsTo` blankDotFooExpected
    it "_ 1 => (\\x -> x 1)" $ do
      let blank1 :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
          blank1 = do
            b <- blank
            one <- number 1
            generalize <$> app b one

          blank1Expected :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
          blank1Expected = do
            v <- var obscureName
            one <- number 1
            a <- app v one
            generalize <$> lam v a
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
      let blankDotFooGroupedBlankBar :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
          blankDotFooGroupedBlankBar = do
            bDotFoo <- blankDotFoo
            b <- blank
            bar <- var "bar"
            a <- app b bar
            g <- grouped bDotFoo
            generalize <$> app g a

          blankDotFooGroupedBlankBarExpected :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
          blankDotFooGroupedBlankBarExpected = do
            v0 <- var obscureName
            v1 <- var "#b"
            bar <- var "bar"
            a <- app v1 bar
            ac <- acc v0 "foo"
            l <- lam v0 ac
            g <- grouped l
            a' <- app g a
            generalize <$> lam v1 a'
      blankDotFooGroupedBlankBar `desugarsTo` blankDotFooGroupedBlankBarExpected
    it "foo bar _ baz => (\\x -> foo bar x baz)" $ do
      let fooBarBlankBaz :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
          fooBarBlankBaz = do
            foo <- var "foo"
            bar <- var "bar"
            baz <- var "baz"
            b <- blank
            a <- app b baz
            a' <- app bar a
            generalize <$> app foo a'

          fooBarBlankBazExpected :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
          fooBarBlankBazExpected = do
            v0 <- var obscureName
            foo <- var "foo"
            bar <- var "bar"
            baz <- var "baz"
            a <- app v0 baz
            a' <- app bar a
            a'' <- app foo a'
            generalize <$> lam v0 a''
      fooBarBlankBaz `desugarsTo` fooBarBlankBazExpected
    it "\\y -> _.foo y => (\\y -> \\x -> x.foo y)" $ do
      let lamYBlankFooY :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
          lamYBlankFooY = do
            y <- var "y"
            b <- blank
            foo <- acc b "foo"
            a <- app foo y
            generalize <$> lam y a

          lamYBlankFooYExpected :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
          lamYBlankFooYExpected = do
            y <- var "y"
            v <- var obscureName
            foo <- acc v "foo"
            a <- app foo y
            l <- lam v a
            generalize <$> lam y l
      lamYBlankFooY `desugarsTo` lamYBlankFooYExpected
    it "_.foo.bar.baz => \\x -> x.foo.bar.baz" $ do
      let blankFooBarBaz :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
          blankFooBarBaz = do
            b <- blank
            foo <- acc b "foo"
            bar <- acc foo "bar"
            generalize <$> acc bar "baz"

          blankFooBarBazExpected :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
          blankFooBarBazExpected = do
            v <- var obscureName
            foo <- acc v   "foo"
            bar <- acc foo "bar"
            baz <- acc bar "baz"
            generalize <$> lam v baz
      blankFooBarBaz `desugarsTo` blankFooBarBazExpected
    it "(_.foo).bar => (\\x -> x.foo).bar" $ do
      let blankFooGroupedBar :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
          blankFooGroupedBar = do
            b <- blank
            foo <- acc b "foo"
            g <- grouped foo
            generalize <$> acc g "bar"

          blankFooGroupedBarExpected :: MonadPassManager m => SubPass BlankDesugaring m (Expr Draft)
          blankFooGroupedBarExpected = do
            v <- var obscureName
            foo <- acc v "foo"
            l <- lam v foo
            g <- grouped l
            generalize <$> acc g "bar"
      blankFooGroupedBar `desugarsTo` blankFooGroupedBarExpected
