{-# LANGUAGE UndecidableInstances #-}

module Luna.Test.Spec.IRSpec where

import Prologue
import Test.Hspec

import qualified Luna.IR.Term.Core  as IR
import qualified Luna.Pass          as Pass
import qualified Luna.Runner        as Luna
import qualified OCI.Pass.Attr      as Attr
import qualified OCI.Pass.Cache     as Pass
import qualified OCI.Pass.Dynamic   as Pass
import qualified OCI.Pass.Scheduler as Scheduler

import OCI.Pass.Definition (Pass)



newtype TestResult = TestResult { checkTestResult :: Expectation }
type instance Attr.Type TestResult = Attr.Atomic
instance Default TestResult where
    def = TestResult (pure ())

data TestPass
type instance Pass.Spec TestPass t = TestPassSpec t
type family   TestPassSpec  t where
    TestPassSpec (Pass.In  Pass.Attrs) = '[TestResult]
    TestPassSpec (Pass.Out Pass.Attrs) = '[TestResult]
    TestPassSpec t                     = Pass.BasicPassSpec t

Pass.cache_phase1 ''TestPass
Pass.cache_phase2 ''TestPass


absolute :: Int -> Int
absolute _ = 8

singleVarTest :: Pass TestPass ()
singleVarTest = do
    v <- IR.var 1
    Attr.put $ TestResult $ 'a' `shouldBe` 'b'
    pure ()


testRunPass :: ∀ pass. (Typeable pass, Pass.Compile pass IO)
            => Pass pass () -> IO TestResult
testRunPass pass = Luna.runManual $ do
    Scheduler.registerAttr     @TestResult
    Scheduler.enableAttrByType @TestResult
    Scheduler.registerPassFromFunction__ pass
    Scheduler.runPassByType @pass
    Scheduler.lookupAttr >>= \case
        -- Nothing -> pure $ TestResult
        --          $ Just "Panic: 'TestResult' attribute not found."
        Just a  -> pure a


spec :: Spec
spec = do
    describe "Terms creation" $ do
        it "Single var" $
            checkTestResult =<< testRunPass singleVarTest

        it "returns a positive number when given a negative input" $
            absolute (-1) `shouldBe` 1

        it "returns zero when given zero" $
            absolute 0 `shouldBe` 0
    pure ()
