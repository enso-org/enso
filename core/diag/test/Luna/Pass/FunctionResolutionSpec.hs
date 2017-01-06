{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module Luna.Pass.FunctionResolutionSpec where

import Luna.Prelude hiding (String)
import qualified Luna.Prelude as P
import Test.Hspec   (Spec, describe, it, shouldBe, shouldReturn)
import Luna.TestUtils
import qualified Luna.IR.Repr.Vis as Vis
import           Luna.Pass        (Pass, SubPass, Inputs, Outputs, Preserves, Events)
import qualified Luna.Pass        as Pass
import Data.TypeDesc
import System.Log
import Luna.IR.Runner
import Luna.IR
import Luna.Pass.Inference.FunctionResolution
import qualified Data.Map          as Map
import Luna.IR.Function.Definition as Function
import Luna.IR.Function
import Luna.IR.Module.Definition   as Module

testImports :: IO Imports
testImports = do
    Right id'    <- runGraph $ do
        bl <- blank
        l  <- lam (arg bl) bl
        compile $ generalize l
    Right const' <- runGraph $ do
        bl1 <- blank
        bl2 <- blank
        li  <- lam (arg bl2) bl1
        l   <- lam (arg bl1) li
        compile $ generalize l
    let mod = Module Map.empty $ Map.fromList [("id", id'), ("const", const')]
    return $ Imports $ Map.singleton "Stdlib" mod

sizeAndCoherence :: (MonadRef m, MonadIO m, MonadPassManager m) => SubPass TestPass m (Int, [Incoherence])
sizeAndCoherence = (,) <$> (length <$> exprs) <*> checkCoherence

initialize :: (MonadRef m, MonadIO m, MonadPassManager m) => P.String -> SubPass TestPass m (Expr (ENT Var (E String) Draft))
initialize name = do
    n   <- string name
    unsafeRelayout <$> var n

runTest m = do
    imps <- testImports
    dropLogs $ runRefCache $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        Right v <- Pass.eval' m
        setAttr (getTypeDesc @Imports) imps
        setAttr (getTypeDesc @CurrentVar) v
        Right res    <- Pass.eval' importVar
        Right (s, c) <- Pass.eval' sizeAndCoherence
        return (res, s, c)


spec :: Spec
spec = do
    describe "successful import" $ do
        it "preserves graph coherence" $ do
            (_, s, c) <- runTest $ initialize "id"
            (s, c) `shouldBe` (8, [])
    describe "unsuccessful import" $ do
        it "returns error and does not change the graph" $ do
            runTest (initialize "foo") `shouldReturn` (Left SymbolNotFound, 4, [])
