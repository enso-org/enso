{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances  #-}

module Luna.Pass.DiscoverySpec (spec) where

import qualified Prelude as P
import Luna.Prelude hiding (String)
import Test.Hspec   (Spec, describe, it, shouldReturn)

import           Luna.IR
import           Luna.Pass (SubPass, Inputs, Outputs, Preserves, Events)
import qualified Luna.Pass        as Pass
import           System.Log


data DiscoveryPass
type instance Abstract         DiscoveryPass = DiscoveryPass

type instance Inputs     Net   DiscoveryPass = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer DiscoveryPass = '[AnyExpr // Model, AnyExprLink // Model]
type instance Inputs     Attr  DiscoveryPass = '[]
type instance Inputs     Event DiscoveryPass = '[]

type instance Outputs    Net   DiscoveryPass = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer DiscoveryPass = '[AnyExpr // Model, AnyExprLink // Model]
type instance Outputs    Attr  DiscoveryPass = '[]
type instance Outputs    Event DiscoveryPass = '[New // AnyExpr, New // AnyExprLink]

type instance Preserves        DiscoveryPass = '[]

sanityPass :: (MonadRef m, MonadIO m, MonadPassManager m) => SubPass DiscoveryPass m P.String
sanityPass = do
    s <- string "hello"
    v <- var s
    match v $ \case
        Var l -> do
            nameNode <- source l
            match nameNode $ \case
                String s -> return s

testCase :: (PrimMonad m, MonadFix m, MonadIO m) => m (Either Pass.InternalError P.String)
testCase = dropLogs $ runRefCache $ evalIRBuilder' $ evalPassManager' $ do
    runRegs
    Pass.eval' sanityPass

spec :: Spec
spec = do
    describe "Discovery Pass" $ do
        it "Preserves basic sanity" $ do
            testCase `shouldReturn` (Right "hello")
