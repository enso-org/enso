module Luna.IR.IRSpec (spec) where

import           Luna.Prelude hiding (String)
import qualified Luna.Prelude as P
import qualified Luna.Pass    as Pass


import Test.Hspec (Spec, describe, it, shouldReturn, shouldBe, expectationFailure, Expectation)

import Luna.IR.Runner
import Luna.IR

data Pair a = Pair a a deriving (Show, Functor, Traversable, Foldable)

pair :: Pair a -> (a,a)
pair (Pair a b) = (a,b)


testInputs :: IO (Either Pass.Err (Pair [AnyExpr]))
testInputs = graphTestCase $ do
    foo  <- rawString "foo"
    bar  <- rawString "bar"
    i1   <- var foo
    i2   <- var bar
    (a :: AnyExpr) <- generalize <$> unify i1 i2
    inps <- inputs a >>= mapM source
    return $ Pair (generalize [i1, i2]) inps

withRight :: Show (Either a b) => Either a b -> (b -> Expectation) -> Expectation
withRight e exp = either (const $ expectationFailure $ "Expected a Right, got: (" <> show e <> ")") exp e

spec :: Spec
spec = do
    describe "inputs" $ do
        it "should return correct inputs" $ do
            answer <- testInputs
            withRight answer $ uncurry shouldBe . pair
