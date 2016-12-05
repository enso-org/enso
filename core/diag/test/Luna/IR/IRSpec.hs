module Luna.IR.IRSpec (spec) where

import           Control.Exception   (PatternMatchFail)
import           Data.Maybe          (isJust)
import           Luna.Prelude        hiding (String)
import qualified Luna.Prelude        as P
import qualified Luna.Pass           as Pass

import Test.Hspec (Spec, describe, it, shouldReturn, shouldBe, shouldSatisfy, expectationFailure, Expectation, shouldThrow, Selector)

import Luna.IR.Runner
import Luna.IR

data Pair a = Pair a a deriving (Show, Functor, Traversable, Foldable)

pair :: Pair a -> (a,a)
pair (Pair a b) = (a,b)

narrowAtom :: forall a m. (IRMonad m, Typeable (AtomOf a), Accessibles m '[ExprLayer Model]) => AnyExpr -> m (Maybe (Expr (AtomOf a)))
narrowAtom expr = do
    exprAtomRep <- getAtomRep expr
    return $ if exprAtomRep == atomRep' @a
      then Just $ unsafeGeneralize expr
      else Nothing

testAtomNarrowing :: IO (Either Pass.Err (Pair (Maybe (Expr Var))))
testAtomNarrowing = graphTestCase $ do
    (foo  :: AnyExpr) <- generalize <$> rawString "foo"
    (vfoo :: AnyExpr) <- generalize <$> var foo
    narrowFoo <- narrowAtom @Var foo
    narrowVar <- narrowAtom @Var vfoo
    return $ Pair narrowFoo narrowVar

testAtomEquality :: IO (Either Pass.Err [Pair Bool])
testAtomEquality = graphTestCase $ do
    (foo  :: AnyExpr) <- generalize <$> rawString "foo"
    (bar  :: AnyExpr) <- generalize <$> rawString "bar"
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

testInputs :: IO (Either Pass.Err (Pair [AnyExpr]))
testInputs = graphTestCase $ do
    foo  <- rawString "foo"
    bar  <- rawString "bar"
    i1   <- var foo
    i2   <- var bar
    (a :: AnyExpr) <- generalize <$> unify i1 i2
    inps <- inputs a >>= mapM source
    return $ Pair (generalize [i1, i2]) inps

crashingPass :: IO (Either Pass.Err ())
crashingPass = graphTestCase $ do
    s <- rawString "foo"
    match s $ \case
        Var s' -> return ()

patternMatchException :: Selector PatternMatchFail
patternMatchException = const True

withRight :: Show (Either a b) => Either a b -> (b -> Expectation) -> Expectation
withRight e exp = either (const $ expectationFailure $ "Expected a Right, got: (" <> show e <> ")") exp e

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
            withRight answer $ \(Pair notVar _  ) -> notVar `shouldBe`      Nothing
            withRight answer $ \(Pair _      var) -> var    `shouldSatisfy` isJust
