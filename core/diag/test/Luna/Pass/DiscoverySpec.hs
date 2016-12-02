{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.DiscoverySpec (spec) where

import qualified Prelude as P
import Luna.Prelude hiding (String, typeRep)
import Test.Hspec   (Spec, describe, it, shouldReturn)

import           Luna.IR
import           Luna.Pass (SubPass, Inputs, Outputs, Preserves)
import qualified Luna.Pass as Pass

data DiscoveryPass
type instance Inputs  DiscoveryPass   = '[ExprNet, ExprLinkNet] <> ExprLayers '[Model] <> ExprLinkLayers '[Model]
type instance Outputs DiscoveryPass   = '[ExprNet, ExprLinkNet] <> ExprLayers '[Model] <> ExprLinkLayers '[Model]
type instance Preserves DiscoveryPass ='[]

sanityPass :: SubPass DiscoveryPass (IRT IO) P.String
sanityPass = layouted @Ent $ do
    v <- var ("hello" :: P.String)
    match v $ \case
        Var l -> do
            nameNode <- source l
            match nameNode $ \case
                String s -> return s

testCase :: IO (Either Pass.Err P.String)
testCase = runIRT $ do
    runRegs
    attachLayer (typeRep @Model) (typeRep @EXPR)
    attachLayer (typeRep @Model) (typeRep @(LINK' EXPR))
    Pass.eval sanityPass


spec :: Spec
spec = do
    describe "Discovery Pass" $ do
        it "Preserves basic sanity" $ do
            testCase `shouldReturn` (Right "hello")
