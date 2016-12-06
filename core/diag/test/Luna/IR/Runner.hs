{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Runner where

import           Luna.Prelude
import           Luna.IR
import           Luna.Pass    (SubPass, Inputs, Outputs, Preserves)
import qualified Luna.Pass    as Pass


data TestPass
type instance Inputs  TestPass   = '[ExprNet, ExprLinkNet] <> ExprLayers '[Model] <> ExprLinkLayers '[Model]
type instance Outputs TestPass   = '[ExprNet, ExprLinkNet] <> ExprLayers '[Model] <> ExprLinkLayers '[Model]
type instance Preserves TestPass ='[]

graphTestCase :: IRMonadBaseIO m => SubPass TestPass (IRBuilder m) a -> m (Either Pass.InternalError a)
graphTestCase pass = evalIRBuilder' $ do
    runRegs
    attachLayer (typeRep' @Model) (typeRep' @EXPR)
    attachLayer (typeRep' @Model) (typeRep' @(LINK' EXPR))
    Pass.eval' pass
