{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Runner (SubPass, TestPass, graphTestCase) where

import           Luna.Prelude
import           Luna.IR
import           Luna.Pass    (SubPass, Inputs, Outputs, Preserves)
import qualified Luna.Pass    as Pass


data TestPass
type instance Inputs  TestPass   = '[ExprNet, ExprLinkNet] <> ExprLayers '[Model] <> ExprLinkLayers '[Model]
type instance Outputs TestPass   = '[ExprNet, ExprLinkNet] <> ExprLayers '[Model] <> ExprLinkLayers '[Model]
type instance Preserves TestPass ='[]

graphTestCase :: SubPass TestPass (IRT IO) a -> IO (Either Pass.InternalError a)
graphTestCase pass = runIRT $ do
    runRegs
    attachLayer (typeRep' @Model) (typeRep' @EXPR)
    attachLayer (typeRep' @Model) (typeRep' @(LINK' EXPR))
    Pass.eval' pass
