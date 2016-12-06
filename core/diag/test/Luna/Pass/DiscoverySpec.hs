{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances  #-}

module Luna.Pass.DiscoverySpec (spec) where

import Control.Exception (PatternMatchFail)
import qualified Prelude as P
import Luna.Prelude hiding (String)
import Test.Hspec   (Selector, Spec, describe, it, shouldReturn, shouldThrow)

import           Luna.IR
import           Luna.Pass (SubPass, Inputs, Outputs, Preserves)
import qualified Luna.Pass as Pass

data DiscoveryPass
type instance Inputs  DiscoveryPass   = '[ExprNet, ExprLinkNet] <> ExprLayers '[Model] <> ExprLinkLayers '[Model]
type instance Outputs DiscoveryPass   = '[ExprNet, ExprLinkNet] <> ExprLayers '[Model] <> ExprLinkLayers '[Model]
type instance Preserves DiscoveryPass ='[]

sanityPass :: SubPass DiscoveryPass (IRT IO) P.String
sanityPass = do
    s <- rawString "hello"
    v <- var s
    match v $ \case
        Var l -> do
            nameNode <- source l
            match nameNode $ \case
                String s -> return s

crashingPass :: SubPass DiscoveryPass (IRT IO) Bool
crashingPass = do
    s <- rawString "foo"
    match s $ \case
        Var s' -> return True

testCase :: IO (Either Pass.InternalError P.String)
testCase = runIRT $ do
    runRegs
    attachLayer (typeRep' @Model) (typeRep' @EXPR)
    attachLayer (typeRep' @Model) (typeRep' @(LINK' EXPR))
    Pass.eval' sanityPass

test :: _ -> IO (Either Pass.InternalError a)
test act = runIRT $ do
    runRegs
    attachLayer (typeRep' @Model) (typeRep' @EXPR)
    attachLayer (typeRep' @Model) (typeRep' @(LINK' EXPR))
    Pass.eval' act

patternMatchException :: Selector PatternMatchFail
patternMatchException = const True

spec :: Spec
spec = do
    describe "Discovery Pass" $ do
        it "Preserves basic sanity" $ do
            testCase `shouldReturn` (Right "hello")
        it "crashes gracefully on match error" $
            test crashingPass `shouldThrow` patternMatchException
