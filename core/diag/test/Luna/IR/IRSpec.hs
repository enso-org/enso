module Luna.IR.IRSpec (spec) where

import           Luna.Prelude hiding (String)
import qualified Luna.Prelude as P

import Test.Hspec (Spec, describe, it, shouldReturn, shouldBe, expectationFailure, Expectation)

import Luna.IR.Runner
import Luna.IR

testInputs = graphTestCase $ do
    i1   <- var ("foo" :: P.String)
    i2   <- var ("bar" :: P.String)
    (a :: Expr (ENT Draft String Draft)) <- unsafeGeneralize <$> unify i1 i2
    inps <- inputs a >>= mapM source
    return (unsafeGeneralize <$> [i1, i2], inps)

withRight :: Show (Either a b) => Either a b -> (b -> Expectation) -> Expectation
withRight e exp = either (const $ expectationFailure $ "Expected a Right, got: (" <> show e <> ")") exp e

spec :: Spec
spec = do
    describe "inputs" $ do
        it "should return correct inputs" $ do
            answer <- testInputs
            withRight answer $ \(correct, result) -> result `shouldBe` correct
