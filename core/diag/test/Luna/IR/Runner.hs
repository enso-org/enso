{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Runner where

import           Luna.Prelude
import           Luna.IR
import           Luna.Pass        (SubPass, Inputs, Outputs, Preserves, Events)
import qualified Luna.Pass        as Pass
import           System.Log
import qualified Luna.IR.Repr.Vis           as Vis
import           Web.Browser                (openBrowser)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Aeson                           (encode)


data TestPass
type instance Abstract  TestPass = TestPass
type instance Inputs    TestPass = '[ExprNet, ExprLinkNet] <> ExprLayers '[Model, Type, Succs, UID] <> ExprLinkLayers '[Model, UID]
type instance Outputs   TestPass = '[ExprNet, ExprLinkNet] <> ExprLayers '[Model, UID]              <> ExprLinkLayers '[Model, UID]
type instance Events    TestPass = '[NEW // EXPR, NEW // LINK' EXPR, DELETE // EXPR, DELETE // LINK' EXPR]
type instance Preserves TestPass ='[]


runGraph :: (pass ~ TestPass, MonadIO m, MonadFix m, PrimMonad m, Pass.KnownDescription pass, Pass.PassInit pass (PassManager (IRBuilder (Logger DropLogger m))))
              => SubPass pass (PassManager (IRBuilder (Logger DropLogger m))) a -> m (Either Pass.InternalError a)
runGraph p = dropLogs $ evalIRBuilder' $ evalPassManager' $ do
    runRegs
    Pass.eval' p

runGraph' :: (MonadIO m, MonadFix m, PrimMonad m, Pass.KnownDescription pass, Pass.PassInit pass (PassManager (IRBuilder (Logger DropLogger m))))
              => SubPass pass (PassManager (IRBuilder (Logger DropLogger m))) a -> m (Either Pass.InternalError a)
runGraph' p = dropLogs $ evalIRBuilder' $ evalPassManager' $ do
    runRegs
    Pass.eval' p

withVis m = do
    (p, vis) <- Vis.newRunDiffT m
    -- putStrLn $ ByteString.unpack $ encode vis
    let cfg = ByteString.unpack $ encode vis
    -- liftIO $ openBrowser $ "http://localhost:8000?cfg=" <> cfg
    return p
