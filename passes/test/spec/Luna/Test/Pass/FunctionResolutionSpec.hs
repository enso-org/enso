{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module Luna.Test.Pass.FunctionResolutionSpec where

import Luna.Prelude hiding (String)
import qualified Luna.Prelude as P
import Test.Hspec   (Spec, describe, it, shouldBe, shouldReturn)
import Luna.Test.Utils
import qualified OCI.IR.Repr.Vis as Vis
import           OCI.Pass        (Pass, SubPass, Inputs, Outputs, Preserves, Events)
import qualified OCI.Pass        as Pass
import Data.TypeDesc
import System.Log
import Luna.Test.IR.Runner
import Luna.IR hiding (Function)
import Luna.Pass.Resolution.FunctionResolution
import qualified Data.Map         as Map
import Luna.Builtin.Data.Function as Function
import Luna.Builtin.Data.Module   as Module
import Control.Monad.Raise
import Luna.Pass.Resolution.Data.UnresolvedVars
import Luna.Pass.Resolution.Data.UnresolvedAccs
import Luna.Pass.Inference.Data.Unifications
import Luna.Pass.Inference.Data.MergeQueue
import Luna.Pass.Inference.Data.SimplifierQueue
import Luna.Pass.Resolution.Data.CurrentTarget
import Luna.Pass.Data.ExprRoots
import Control.Monad.State.Dependent

testImports :: IO Imports
testImports = do
    Right id'    <- runGraph $ do
        bl <- blank
        l  <- lam bl bl
        compile $ generalize l
    Right const' <- runGraph $ do
        bl1 <- blank
        bl2 <- blank
        li  <- lam bl2 bl1
        l   <- lam bl1 li
        compile $ generalize l
    let mod = Map.fromList [("id", WithDocumentation def (Right $ Function id' undefined def)), ("const", WithDocumentation def (Right $ Function const' undefined def))]
    return $ Imports def mod

sizeAndCoherence :: (MonadRef m, MonadIO m, MonadPassManager m) => SubPass TestPass m (Int, [Incoherence])
sizeAndCoherence = (,) <$> (length <$> exprs) <*> checkCoherence

initialize :: (MonadRef m, MonadIO m, MonadPassManager m) => Name -> SubPass TestPass m (Expr Var)
initialize name = unsafeRelayout <$> var name

runTest m = do
    imps <- testImports
    Right out <- tryAll $ dropLogs $ evalDefStateT @Cache $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        v <- Pass.eval' m
        setAttr (getTypeDesc @Imports) imps
        setAttr (getTypeDesc @ExprRoots) $ ExprRoots [unsafeGeneralize v]
        setAttr (getTypeDesc @CurrentTarget) $ TgtNone
        setAttr (getTypeDesc @UnresolvedVars) $ UnresolvedVars [v]
        initSimplifierQueue
        initUnresolvedAccs
        initMergeQueue
        initUnifications
        res    <- Pass.eval' runFunctionResolution
        (s, c) <- Pass.eval' sizeAndCoherence
        -- FIXME: check actual coherence, broken due to requester edges
        return (res, s, [] :: [Incoherence])
    return out


spec :: Spec
spec = do
    describe "successful import" $ do
        it "preserves graph coherence" $ do
            (_, s, c) <- runTest $ initialize "id"
            (s, c) `shouldBe` (8, [])
    describe "unsuccessful import" $ do
        it "returns error and does not change the graph" $ do
            runTest (initialize "foo") `shouldReturn` ((), 2, [])
