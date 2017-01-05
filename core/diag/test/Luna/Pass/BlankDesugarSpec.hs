{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wall -Wno-partial-type-signatures #-}

module Luna.Pass.BlankDesugarSpec (spec) where

import           Luna.Pass        (SubPass, Inputs, Outputs, Preserves, Events, setAttr3)
import qualified Luna.Pass        as Pass

import Test.Hspec   (Spec, Expectation, describe, it, shouldBe, shouldSatisfy)
import Luna.Prelude hiding (String, s, new)
import qualified Luna.Prelude as P
import Data.TypeDesc
import qualified Luna.IR.Repr.Vis as Vis
import Luna.TestUtils
import Luna.IR.Expr.Combinators
import Luna.IR.Function hiding (args)
import Luna.IR.Runner
import Luna.IR.Expr.Layout.ENT hiding (Cons)
import Luna.IR
import System.Log
import Control.Monad (foldM)

newtype UniqueNameGen = UniqueNameGen (P.String, Int)

newtype UsedVars = UsedVars [Expr $ Var #> String]

genName :: (MonadIR m) => SubPass BlankDesugaring m P.String
genName = do
    UniqueNameGen (base, number) <- readAttr
    setAttr3 $ UniqueNameGen (base, number + 1)
    return $ '^' : base ++ show number

obscureName :: P.String
obscureName = "^obscureName0"

data BlankDesugaring
type instance Abstract   BlankDesugaring = BlankDesugaring
type instance Inputs     Net   BlankDesugaring = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer BlankDesugaring = '[AnyExpr // Model, AnyExprLink // Model, AnyExpr // Type, AnyExpr // Succs]
type instance Inputs     Attr  BlankDesugaring = '[UniqueNameGen, UsedVars]
type instance Inputs     Event BlankDesugaring = '[]

type instance Outputs    Net   BlankDesugaring = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer BlankDesugaring = '[AnyExpr // Model, AnyExprLink // Model, AnyExpr // Succs, AnyExpr // Type]
type instance Outputs    Attr  BlankDesugaring = '[UniqueNameGen, UsedVars]
type instance Outputs    Event BlankDesugaring = '[New // AnyExpr, New // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink]

type instance Preserves        BlankDesugaring = '[]

blankDotFoo :: _ => SubPass BlankDesugaring _ _
blankDotFoo = do
    b <- blank
    rawAcc "foo" b

blankDotFooExpected :: _ => SubPass BlankDesugaring _ _
blankDotFooExpected = do
    v <- strVar obscureName
    a <- rawAcc "foo" v
    lam (arg v) a

localAttr :: forall attr pass m a. _ => _ -> SubPass pass m a -> SubPass pass m a
localAttr newAttr act = do
    st <- readAttr @attr
    setAttr3 @attr newAttr
    res <- act
    setAttr3 @attr st
    return res

safeReplaceNode :: _ => SomeExpr -> SomeExpr -> SubPass BlankDesugaring m ()
safeReplaceNode old new = do
    b <- generalize <$> blank
    replaceNode old b
    replaceNode b new
    deleteSubtree b

desugar :: forall m. (MonadIR m, MonadPassManager m, _)
        => SomeExpr -> SubPass BlankDesugaring m SomeExpr
desugar e = do
    e'      <- replaceBlanks e
    UsedVars vars    <- readAttr @UsedVars
    newExpr <- lams (map unsafeRelayout $ reverse vars) e'
    safeReplaceNode e newExpr
    return newExpr

modifyAttr :: forall attr pass m. _ => (_ -> _) -> SubPass pass m ()
modifyAttr f = do
    st <- readAttr @attr
    setAttr3 @attr $ f st

replaceBlanks :: forall m. (MonadIR m, MonadPassManager m)
              => SomeExpr -> SubPass BlankDesugaring m SomeExpr
replaceBlanks e = match e $ \case
    -- interesting cases:

    -- blank is replaced by new name var and this var is saved
    -- for reuse in lambda
    Blank -> do
        n <- genName
        v <- strVar n
        safeReplaceNode e $ generalize v
        modifyAttr $ \(UsedVars s) -> UsedVars (v:s)
        deleteSubtree e
        return $ unsafeRelayout v
    -- grouped starts new desugaring environment
    Grouped g -> do
        g' <- source g
        desu <- localAttr (UsedVars []) $ desugar g'
        unsafeRelayout <$> grouped desu
    Lam (Arg _ v) f -> do
        v' <- source v
        f' <- source f >>= localAttr (UsedVars []) . desugar
        unsafeRelayout <$> lam (arg v') f'

    -- these just cut through constructors
    App f (Arg _ a) -> do
        f' <- source f >>= replaceBlanks
        a' <- source a >>= replaceBlanks
        unsafeRelayout <$> app f' (arg a')
    Acc n v -> do
        n' <- source n
        v' <- source v >>= replaceBlanks
        unsafeRelayout <$> acc n' v'
    Integer{} -> return e
    Rational{} -> return e
    String{} -> return e
    Cons{} -> return e
    Var{} -> return e
    Star -> return e
    Missing -> return e
    Unify{} -> return e

lams :: _ => [SomeExpr] -> SomeExpr -> m SomeExpr
lams args output = unsafeRelayout <$> foldM f (unsafeRelayout output) (unsafeRelayout <$> reverse args)
    where
        f arg' lam' = lamAny (arg lam') arg'

lamAny :: _ => Arg SomeExpr -> SomeExpr -> m SomeExpr
lamAny a b = fmap generalize $ lam a b

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
    (res, coherence, blanks) <- withVis $ dropLogs $ runRefCache $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        Right x <- Pass.eval' test
        void $ Pass.eval' $ snapshotVis "start"
        setAttr (getTypeDesc @UniqueNameGen) $ UniqueNameGen ("obscureName", (0::Int))
        setAttr (getTypeDesc @UsedVars) $ UsedVars []
        Right desugared <- Pass.eval' $ desugar $ generalize x
        Right coherence <- Pass.eval' @BlankDesugaring checkCoherence
        Right blanks    <- Pass.eval' noBlankLeftBehind
        void $ Pass.eval' $ snapshotVis "desugar"
        Right expected' <- Pass.eval' expected
        void $ Pass.eval' $ snapshotVis "expected"
        Right result <- Pass.eval' $ areExpressionsIsomorphic @(SubPass BlankDesugaring _) (unsafeRelayout expected') (unsafeRelayout desugared)
        return (result, coherence, blanks)
    res `shouldBe` True
    coherence `shouldSatisfy` null
    blanks `shouldBe` False

replacesTo :: _ => _ -> _ -> Expectation
replacesTo test expected = do
    res <- dropLogs $ runRefCache $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        Right x <- Pass.eval' test
        setAttr (getTypeDesc @UniqueNameGen) $ UniqueNameGen ("obscureName", (0::Int))
        setAttr (getTypeDesc @UsedVars) $ UsedVars []
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
    app1 <- app b (arg seven)
    app foo (arg app1)

fooBlank7Replaced :: _ => SubPass BlankDesugaring _ _
fooBlank7Replaced = do
    foo <- strVar "foo"
    v <- strVar obscureName
    seven <- integer (7::Int)
    app1 <- app v (arg seven)
    app foo (arg app1)

fooBlank7Expected :: _ => SubPass BlankDesugaring _ _
fooBlank7Expected = do
    foo <- strVar "foo"
    v <- strVar obscureName
    seven <- integer (7::Int)
    app1 <- app v (arg seven)
    app2 <- app foo (arg app1)
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
