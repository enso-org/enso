{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Runner (SubPass, TestPass, graphTestCase) where

import           Luna.Prelude
import           Luna.IR
import           Luna.Pass    (SubPass, Inputs, Outputs, Preserves, Events)
import qualified Luna.Pass    as Pass
import           System.Log


data TestPass
type instance Abstract  TestPass = TestPass
type instance Inputs    TestPass = '[ExprNet, ExprLinkNet] <> ExprLayers '[Model, Type, Succs, UID] <> ExprLinkLayers '[Model, UID]
type instance Outputs   TestPass = '[ExprNet, ExprLinkNet] <> ExprLayers '[Model, UID]              <> ExprLinkLayers '[Model, UID]
type instance Events    TestPass = '[NEW // EXPR, NEW // LINK' EXPR, DELETE // EXPR, DELETE // LINK' EXPR]
type instance Preserves TestPass ='[]


graphTestCase :: (pass ~ TestPass, MonadIO m, MonadFix m, PrimMonad m, Pass.KnownDescription pass, Pass.PassInit pass (PassManager (IRBuilder (Logger DropLogger m))))
              => SubPass pass (PassManager (IRBuilder (Logger DropLogger m))) a -> m (Either Pass.InternalError a)
graphTestCase p = dropLogs $ evalIRBuilder' $ evalPassManager' $ do
    runRegs
    Pass.eval' p
