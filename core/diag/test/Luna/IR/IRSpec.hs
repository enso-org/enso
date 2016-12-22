module Luna.IR.IRSpec (spec) where

import           Control.Exception   (PatternMatchFail)
import           Data.Maybe          (isJust)
import           Luna.Prelude        hiding (String)
import qualified Luna.Prelude        as P
import qualified Luna.Pass           as Pass

import Test.Hspec (Spec, describe, it, shouldReturn, shouldBe, shouldSatisfy, expectationFailure, Expectation, shouldThrow, Selector)

import Luna.IR.Runner
import Luna.IR
import Luna.TestUtils
import Luna.IR.Expr.Term        (Term(Sym_String, Sym_Unify))
import Luna.IR.Expr.Combinators
import Luna.IR.Function.Class

data Pair a = Pair a a deriving (Show, Functor, Traversable, Foldable)

pair :: Pair a -> (a,a)
pair (Pair a b) = (a,b)

testCompile :: IO (Either Pass.InternalError CompiledFunction)
testCompile = graphTestCase $ do
    t <- string "foobar"
    v <- var t
    a <- acc t v
    compile (generalize a)

testImport :: IO (Either Pass.InternalError (Int, [Incoherence]))
testImport = do
    Right f <- testCompile
    graphTestCase $ do
        string "foo"
        string "baz"
        importFunction f
        size <- length <$> exprs
        coh  <- checkCoherence
        return (size, coh)

changeStringLiteral s (Sym_String _) = Sym_String s

testVarRenaming :: IO (Either Pass.InternalError P.String)
testVarRenaming = graphTestCase $ do
    (v :: Expr Draft) <- generalize <$> strVar "foo"
    match v $ \case
        Var n -> do
            (name :: Expr (E String)) <- unsafeGeneralize <$> source n
            modifyExprTerm name $ changeStringLiteral "bar"
    match v $ \case
        Var n -> do
            name <- source n
            match name $ \case
                String s -> return s


testAtomNarrowing :: IO (Either Pass.InternalError (Pair (Maybe (Expr Var))))
testAtomNarrowing = graphTestCase $ do
    (foo  :: AnyExpr) <- generalize <$> string "foo"
    (vfoo :: AnyExpr) <- generalize <$> var foo
    narrowFoo <- narrowAtom @Var foo
    narrowVar <- narrowAtom @Var vfoo
    return $ Pair narrowFoo narrowVar

testAtomEquality :: IO (Either Pass.InternalError [Pair Bool])
testAtomEquality = graphTestCase $ do
    (foo  :: AnyExpr) <- generalize <$> string "foo"
    (bar  :: AnyExpr) <- generalize <$> string "bar"
    (vfoo :: AnyExpr) <- generalize <$> var foo
    (vbar :: AnyExpr) <- generalize <$> var bar
    (uni  :: AnyExpr) <- generalize <$> unify vfoo vbar
    sameFooBar   <- isSameAtom foo  bar
    sameFooFoo   <- isSameAtom foo  foo
    sameFooUni   <- isSameAtom foo  uni
    sameVFooUni  <- isSameAtom vfoo uni
    sameVFooVBar <- isSameAtom vfoo vbar
    sameUniUni   <- isSameAtom uni  uni
    return $ [ Pair sameFooBar   True
             , Pair sameFooFoo   True
             , Pair sameFooUni   False
             , Pair sameVFooUni  False
             , Pair sameVFooVBar True
             , Pair sameUniUni   True
             ]

testInputs :: IO (Either Pass.InternalError (Pair [AnyExpr]))
testInputs = graphTestCase $ do
    foo  <- string "foo"
    bar  <- string "bar"
    i1   <- var foo
    i2   <- var bar
    (a :: AnyExpr) <- generalize <$> unify i1 i2
    inps <- inputs a >>= mapM source
    return $ Pair (generalize [i1, i2]) inps

crashingPass :: IO (Either Pass.InternalError ())
crashingPass = graphTestCase $ do
    s <- string "foo"
    match s $ \case
        Var s' -> return ()

patternMatchException :: Selector PatternMatchFail
patternMatchException = const True

testNodeRemovalCoherence :: IO (Either Pass.InternalError [Incoherence])
testNodeRemovalCoherence = graphTestCase $ do
    foo   <- string "foo"
    bar   <- string "bar"
    vfoo  <- var foo
    vbar  <- var bar
    vbar' <- var bar
    uni   <- unify vfoo vbar
    delete vbar'
    delete uni
    checkCoherence

testSubtreeRemoval :: IO (Either Pass.InternalError (Int, [Incoherence]))
testSubtreeRemoval = graphTestCase $ do
    foo   <- string "foo"
    bar   <- string "bar"
    vfoo  <- var foo
    vbar  <- var bar
    vbar' <- var bar
    uni   <- unify vfoo vbar
    deleteSubtree $ generalize uni
    -- Here we expect the graph to be coherent and contain only vbar' and bar and their types
    coh <- checkCoherence
    exs <- length <$> exprs
    return (exs, coh)

testChangeSource :: IO (Either Pass.InternalError (Bool, [Incoherence]))
testChangeSource = graphTestCase $ do
    foo <- string "foo"
    bar <- string "bar"
    baz <- string "baz"
    uni <- unify foo bar
    match uni $ \(Unify l r) -> changeSource (generalize r) (generalize baz)
    rightOperand :: AnyExpr <- fmap generalize $ match uni $ \(Unify l r) -> source r
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
            answer <- testAtomEquality
            withRight answer $ flip shouldSatisfy $ and . fmap (uncurry (==) . pair)
    describe "errors behaviour" $ do
        it "crashes gracefully on a match error" $ do
            crashingPass `shouldThrow` patternMatchException
    describe "atom narrowing" $ do
        it "correctly detects the type of an expression" $ do
            answer <- testAtomNarrowing
            withRight answer $ flip shouldBe      Nothing . fst . pair
            withRight answer $ flip shouldSatisfy isJust  . snd . pair
    describe "var renaming" $ do
        it "changes the variable name in place" $ do
            testVarRenaming `shouldReturn` Right "bar"
    describe "node removal" $ do
        it "preserves graph coherence" $
            testNodeRemovalCoherence `shouldReturn` Right []
    describe "subtree removal" $ do
        it "removes a subgraph while preserving coherence" $
            testSubtreeRemoval `shouldReturn` Right (4, [])
    describe "changing edge source" $ do
        it "changes the source and preserves coherence" $
            testChangeSource `shouldReturn` Right (True, [])
    describe "function importing" $ do
        it "imports function into current grpah" $
            testImport `shouldReturn` Right (10, [])
