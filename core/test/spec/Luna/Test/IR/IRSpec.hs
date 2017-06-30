{-# LANGUAGE OverloadedStrings #-}

module Luna.Test.IR.IRSpec (spec) where

import           Control.Exception   (PatternMatchFail)
import           Data.Maybe          (isJust)
import           Luna.Prelude        hiding (String)
import qualified Luna.Prelude        as P
import qualified OCI.Pass           as Pass
import qualified OCI.IR.Repr.Vis    as Vis

import Test.Hspec

import Luna.Test.IR.Runner
import Luna.IR
import Luna.Test.Utils
import OCI.IR.Combinators
import Luna.Builtin.Data.Function

data Pair a = Pair a a deriving (Show, Functor, Traversable, Foldable)

pair :: Pair a -> (a,a)
pair (Pair a b) = (a,b)

testReconnectLayer :: IO (Either SomeException (Bool, [Incoherence]))
testReconnectLayer = runGraph $ do
    one <- number 1
    c   <- cons_ @Draft "Int"
    reconnectLayer @Type c one
    coh <- checkCoherence
    tp  <- getLayer @Type one >>= source
    return (tp == generalize c, coh)

testCompile :: IO (Either SomeException (Rooted SomeExpr))
testCompile = runGraph $ do
    v <- var "foobar"
    a <- acc v "foobar"
    compile (generalize a)

testImport :: IO (Either SomeException (Int, [Incoherence]))
testImport = do
    Right f <- testCompile
    runGraph $ do
        var "foo"
        var "baz"
        importRooted f
        size <- length <$> exprs
        coh  <- checkCoherence
        return (size, coh)

testVarRenaming :: IO (Either SomeException Name)
testVarRenaming = runGraph $ do
    v :: Expr Var <- generalize <$> var "foo"
    modifyExprTerm v $ name .~ "bar"
    matchExpr v $ \case
        Var n -> return n

testTermNarrowing :: IO (Either SomeException (Pair (Maybe (Expr Var))))
testTermNarrowing = runGraph $ do
    (bl :: SomeExpr) <- generalize <$> blank
    (v  :: SomeExpr) <- generalize <$> var "foo"
    narrowBl <- narrowTerm @Var bl
    narrowV  <- narrowTerm @Var v
    return $ Pair narrowBl narrowV

testTermEquality :: IO (Either SomeException [Pair Bool])
testTermEquality = runGraph $ do
    (foo  :: SomeExpr) <- generalize <$> blank
    (bar  :: SomeExpr) <- generalize <$> blank
    (vfoo :: SomeExpr) <- generalize <$> var "foo"
    (vbar :: SomeExpr) <- generalize <$> var "bar"
    (uni  :: SomeExpr) <- generalize <$> unify vfoo vbar
    sameFooBar   <- isSameTerm foo  bar
    sameFooFoo   <- isSameTerm foo  foo
    sameFooUni   <- isSameTerm foo  uni
    sameVFooUni  <- isSameTerm vfoo uni
    sameVFooVBar <- isSameTerm vfoo vbar
    sameUniUni   <- isSameTerm uni  uni
    return $ [ Pair sameFooBar   True
             , Pair sameFooFoo   True
             , Pair sameFooUni   False
             , Pair sameVFooUni  False
             , Pair sameVFooVBar True
             , Pair sameUniUni   True
             ]

testInputs :: IO (Either SomeException (Pair [SomeExpr]))
testInputs = runGraph $ do
    i1 <- var "foo"
    i2 <- var "bar"
    (a :: SomeExpr) <- generalize <$> unify i1 i2
    inps <- inputs a >>= mapM source
    return $ Pair (generalize [i1, i2]) inps

crashingPass :: IO (Either SomeException ())
crashingPass = runGraph $ do
    s <- string "foo"
    matchExpr s $ \case
        Var s' -> return ()

patternMatchException :: Selector PatternMatchFail
patternMatchException = const True

testNodeRemovalCoherence :: IO (Either SomeException [Incoherence])
testNodeRemovalCoherence = runGraph $ do
    foo <- var "foo"
    delete foo
    checkCoherence

testSubtreeRemoval :: IO (Either SomeException (Int, [Incoherence]))
testSubtreeRemoval = runGraph $ do
    vfoo  <- var "foo"
    vbar  <- var "bar"
    vbar' <- var "bar"
    uni   <- unify vfoo vbar
    deleteSubtree uni
    -- Here we expect the graph to be coherent and contain only vbar' and bar and their types
    coh <- checkCoherence
    exs <- length <$> exprs
    return (exs, coh)

testSubtreeRemovalWithNodeDuplications :: IO (Either SomeException (Int, [Incoherence]))
testSubtreeRemovalWithNodeDuplications = runGraph $ do
    v  <- var "foo"
    l  <- lam v v
    v2 <- var "bar"
    u  <- unify v2 l
    deleteSubtree u
    coh <- checkCoherence
    exs <- length <$> exprs
    return (exs, coh)

testChangeSource :: IO (Either SomeException (Bool, [Incoherence]))
testChangeSource = runGraph $ do
    foo <- string "foo"
    bar <- string "bar"
    baz <- string "baz"
    uni <- unify foo bar
    matchExpr uni $ \(Unify l r) -> replaceSource baz r
    rightOperand :: SomeExpr <- fmap generalize $ matchExpr uni $ \(Unify l r) -> source r
    coh <- checkCoherence
    return (rightOperand == generalize baz, coh)


spec :: Spec
spec = do
    describe "inputs" $ do
        it "should return correct inputs" $ do
            answer <- testInputs
            withRight answer $ uncurry shouldBe . pair
    describe "atom equality" $ do
        it "should check whether terms are based on the same atom" $ do
            answer <- testTermEquality
            withRight answer $ flip shouldSatisfy $ and . fmap (uncurry (==) . pair)
    describe "errors behaviour" $ do
        it "crashes gracefully on a matchExpr error" $ do
            crashingPass `shouldThrow` patternMatchException
    describe "atom narrowing" $ do
        it "correctly detects the type of an expression" $ do
            answer <- testTermNarrowing
            withRight answer $ flip shouldBe      Nothing . fst . pair
            withRight answer $ flip shouldSatisfy isJust  . snd . pair
    describe "var renaming" $ do
        it "changes the variable name in place" $ do
            flip withRight (`shouldBe` "bar") =<< testVarRenaming
    describe "node removal" $ do
        it "preserves graph coherence" $ do
            flip withRight (`shouldBe` []) =<< testNodeRemovalCoherence
    describe "subtree removal" $ do
        it "removes a subgraph while preserving coherence" $
            flip withRight (`shouldBe` (2, [])) =<< testSubtreeRemoval
        it "works with duplicated usage of nodes" $ do
            flip withRight (`shouldBe` (0, [])) =<< testSubtreeRemovalWithNodeDuplications
    describe "changing edge source" $ do
        it "changes the source and preserves coherence" $
            flip withRight (`shouldBe` (True, [])) =<< testChangeSource
    describe "function importing" $ do
        it "imports function into current grpah" $
            flip withRight (`shouldBe` (8, [])) =<< testImport
    describe "layer reconnect" $
        it "changes the layer pointer and preserves coherence" $
            flip withRight (`shouldBe` (True, [])) =<< testReconnectLayer
