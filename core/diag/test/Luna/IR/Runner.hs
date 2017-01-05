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
import System.Log.Logger.Format (nestedColorFormatter)


data TestPass
type instance Abstract TestPass = TestPass
type instance Inputs  Net   TestPass = '[AnyExpr, AnyExprLink]
type instance Outputs Net   TestPass = '[AnyExpr, AnyExprLink]
type instance Inputs  Layer TestPass = '[AnyExpr // Model, AnyExpr // UID, AnyExpr // Type, AnyExpr // Succs, AnyExprLink // UID, AnyExprLink // Model]
type instance Outputs Layer TestPass = '[AnyExpr // Model, AnyExpr // UID, AnyExpr // Type, AnyExpr // Succs, AnyExprLink // UID, AnyExprLink // Model]
type instance Inputs  Attr  TestPass = '[]
type instance Outputs Attr  TestPass = '[]
type instance Inputs  Event TestPass = '[] -- will never be used
type instance Outputs Event TestPass = '[New // AnyExpr, New // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink, Import // AnyExpr, Import // AnyExprLink]
type instance Preserves     TestPass = '[]


runGraph :: (pass ~ TestPass, MonadIO m, MonadFix m, PrimMonad m, Pass.KnownDescription pass, Pass.PassInit pass (PassManager (IRBuilder (RefCache (Logger DropLogger m)))))
              => SubPass pass (PassManager (IRBuilder (RefCache (Logger DropLogger m)))) a -> m (Either Pass.InternalError a)
runGraph p = dropLogs $ runRefCache $ evalIRBuilder' $ evalPassManager' $ do
    runRegs
    Pass.eval' p

-- runLoggedGraph :: (pass ~ TestPass, MonadIO m, MonadFix m, PrimMonad m, Pass.KnownDescription pass)
--                => SubPass pass (PassManager (IRBuilder (RefCache _))) a -> m (Either Pass.InternalError a)
runLoggedGraph (eqTTestPass -> p) = runTaggedLogging $ runEchoLogger $ runFormatLogger nestedColorFormatter $ runRefCache $ evalIRBuilder' $ evalPassManager' $ do
    runRegs
    Pass.eval' p

eqTTestPass :: SubPass TestPass m a -> SubPass TestPass m a
eqTTestPass = id

runGraph' :: (MonadIO m, MonadFix m, PrimMonad m, Pass.KnownDescription pass, Pass.PassInit pass (PassManager (IRBuilder (Logger DropLogger m))))
              => SubPass pass (PassManager (IRBuilder (RefCache (Logger DropLogger m)))) a -> m (Either Pass.InternalError a)
runGraph' p = dropLogs $ runRefCache $ evalIRBuilder' $ evalPassManager' $ do
    runRegs
    Pass.eval' p

withVis m = do
    (p, vis) <- Vis.newRunDiffT m
    -- putStrLn $ ByteString.unpack $ encode vis
    let cfg = ByteString.unpack $ encode vis
    -- liftIO $ openBrowser $ "http://localhost:8000?cfg=" <> cfg
    return p
