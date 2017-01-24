{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.AccessorFunctionSpec (spec) where

import           Luna.Pass hiding (compile)
import qualified Luna.Pass        as Pass
import           Control.Monad.Raise (MonadException(..), tryAll)
import qualified Luna.IR.Repr.Vis as Vis

import qualified Data.Set as Set (null)
import Test.Hspec   (Spec, Expectation, describe, expectationFailure, it, shouldBe, shouldSatisfy, shouldMatchList)
import Luna.Prelude hiding (String, s, new)
import qualified Luna.Prelude as P
import Data.Maybe (isJust)
import Data.TypeDesc
import qualified Luna.IR.Repr.Vis as Vis
import Luna.IR.Expr.Combinators
import Luna.IR.Imports
import qualified Luna.IR.Module.Definition   as Module
import qualified Data.Map          as Map
import Luna.IR.Function hiding (args)
import Luna.IR.Function.Definition
import Luna.IR.Expr.Layout
import Luna.IR.Layer.Redirect
import Luna.Pass.Sugar.TH (makePass)
import Luna.IR.Expr.Layout.ENT hiding (Cons)
import           Luna.IR.Name                (Name)
import Luna.IR.Class.Method        (Method(..))
import Luna.IR.Class.Definition
import Luna.IR.Runner
import Luna.Pass.Sugar.Construction
import Luna.IR
import Luna.TestUtils
import Luna.Pass.Inference.MethodResolution
import Luna.Pass.Inference.FunctionResolution (ImportError(..), lookupSym)
import System.Log
import Control.Monad (foldM)
import Type.Any (AnyType)



testSuccess :: _ => SubPass TestPass _ _
testSuccess = do
    one <- integer (1::Int)
    int <- string "Int"
    c <- cons_ @Draft int
    reconnectLayer @Type c one

    rawAcc "succ" one

testUnknownMethod :: _ => SubPass TestPass _ _
testUnknownMethod = do
    one <- integer (1::Int)
    int <- string "Int"
    c <- cons_ @Draft int
    reconnectLayer @Type c one

    rawAcc "isLetter" one

testAmbiguousType :: _ => SubPass TestPass _ _
testAmbiguousType = do
    one <- integer (1::Int)
    int <- string "Int"

    rawAcc "isLetter" one

testImports :: IO Imports
testImports = do
    Right succ' <- runGraph $ do
        self <- strVar "self"
        one <- integer (1::Int)
        plus <- strVar "+"
        a1 <- app plus (arg self)
        a2 <- app a1 (arg one)
        c <- compile $ generalize a2
        return $ Method (generalize self) c
    let klass = Class Map.empty $ Map.fromList [("succ", succ')]
    let mod = Module.Module (Map.fromList [("Int", klass)]) Map.empty
    return $ Imports $ Map.singleton "Stdlib" mod

instance Exception e => MonadException e IO where
    raise = throwM

unifies :: _ => SubPass AccessorFunction _ [(SomeExpr, SomeExpr)]
unifies = do
    es <- exprs
    maybeUnifies <- mapM (narrowAtom @Unify) es
    let unifies = catMaybes maybeUnifies
    forM unifies $ flip match $ \case
        Unify l r -> do
            t <- (,) <$> source l <*> source r
            return $ over each generalize t

snapshotVis :: (MonadIR m, Vis.MonadVis m, MonadRef m) => P.String -> Pass.Pass TestPass m
snapshotVis = Vis.snapshot

runTest m = do
    imps <- testImports
    out <- withVis $ dropLogs $ runRefCache $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        addExprEventListener @Redirect initRedirectPass
        attachLayer 20 (getTypeDesc @Redirect) (getTypeDesc @AnyExpr)
        v <- Pass.eval' m
        setAttr (getTypeDesc @Imports) imps
        setAttr (getTypeDesc @CurrentAcc) v
        res    <- Pass.eval' importAccessor'
        void $ Pass.eval' $ snapshotVis "import"
        c      <- Pass.eval' @AccessorFunction checkCoherence
        (redirect :: Maybe SomeExpr) <- Pass.eval' @AccessorFunction $ do
            l <- readLayer @Redirect v
            case l of
                Just l' -> do
                    src <- source l'
                    return $ Just $ generalize src
                _       -> return Nothing
        allUnifies  <- Pass.eval' @AccessorFunction unifies
        unifiesAndSuccs <- forM res $ \(self, body) -> Pass.eval' @AccessorFunction $ do
            accType  <- readLayer @Type v    >>= source
            bodyType <- readLayer @Type body >>= source
            let accBodyUnify :: (SomeExpr, SomeExpr)
                accBodyUnify = (generalize accType, generalize bodyType)

            selfType <- readLayer @Type self >>= source
            accTargetType <- match v $ \case
                Acc _ target -> source target >>= readLayer @Type >>= source
            let accSelfUnify :: (SomeExpr, SomeExpr)
                accSelfUnify = (generalize selfType, generalize accTargetType)
            selfSuccs <- readLayer @Succs self
            return $ (selfSuccs, [accBodyUnify, accSelfUnify])

        return (res, c, redirect, allUnifies, unifiesAndSuccs)
    return out

spec :: Spec
spec = describe "accessor function importer" $ do
    it "imports" $ do
        (res, coherence, redirect, allUnifies, unifiesAndSuccs) <- runTest testSuccess
        withRight res $ \(_self, body) -> do
            redirect `shouldBe` Just body
        coherence `shouldSatisfy` null
        withRight unifiesAndSuccs $ \(selfSuccs, unifies) -> do
            selfSuccs `shouldSatisfy` Set.null
            unifies `shouldMatchList` allUnifies
    it "does not import unknown method" $ do
        (res, coherence, redirect, allUnifies, unifiesAndSuccs) <- runTest testUnknownMethod
        res `shouldBe` Left (MethodNotFound "isLetter")
        coherence `shouldSatisfy` null
    it "does not import when type is ambiguous" $ do
        (res, coherence, redirect, allUnifies, unifiesAndSuccs) <- runTest testAmbiguousType
        res `shouldBe` Left AmbiguousType
        coherence `shouldSatisfy` null
