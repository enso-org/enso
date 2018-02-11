{-# LANGUAGE UndecidableInstances #-}

module Luna.Test.IR.Runner where

import           Luna.Prelude
import           Luna.IR
import qualified OCI.IR.Class as Event
import           OCI.Pass        (SubPass, Inputs, Outputs, Preserves, Events)
import qualified OCI.Pass        as Pass
import           System.Log
import qualified OCI.IR.Repr.Vis           as Vis
import           Web.Browser                (openBrowser)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Maybe (isJust)
import Data.Aeson                           (encode)
import System.Log.Logger.Format (nestedColorFormatter, bulletNestingFormatter)
import Control.Monad.Raise
import System.Environment (lookupEnv)
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID    as UUID
import Control.Monad.State.Dependent

data TestPass
type instance Abstract TestPass = TestPass
type instance Inputs  Net   TestPass = '[AnyExpr, AnyExprLink]
type instance Outputs Net   TestPass = '[AnyExpr, AnyExprLink]
type instance Inputs  Layer TestPass = '[AnyExpr // Model, AnyExpr // UID, AnyExpr // Type, AnyExpr // UserType, AnyExpr // Errors, AnyExpr // RequiredBy, AnyExpr // Succs, AnyExprLink // UID, AnyExprLink // Model]
type instance Outputs Layer TestPass = '[AnyExpr // Model, AnyExpr // UID, AnyExpr // Type, AnyExpr // UserType, AnyExpr // Errors, AnyExpr // RequiredBy, AnyExpr // Succs, AnyExprLink // UID, AnyExprLink // Model]
type instance Inputs  Attr  TestPass = '[]
type instance Outputs Attr  TestPass = '[]
type instance Inputs  Event TestPass = '[] -- will never be used
type instance Outputs Event TestPass = '[New // AnyExpr, New // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink, Event.Import // AnyExpr, Event.Import // AnyExprLink, OnDeepDelete // AnyExpr]
type instance Preserves     TestPass = '[]


type PMStack m = PassManager (IRBuilder (StateT Cache (Logger DropLogger (Vis.VisStateT (ExceptT' m)))))

{-runPMWithLogs :: MonadIO m => Bool -> PMStack m a -> m (Either SomeException a)-}
runPMWithLogs vis = tryAll . (if vis then withVis else silenceVis) . runTaggedLogging . runEchoLogger . runFormatLogger bulletNestingFormatter . evalDefStateT @Cache . evalIRBuilder' . evalPassManager'

runPM :: MonadIO m => Bool -> PMStack m a -> m (Either SomeException a)
runPM vis = tryAll . (if vis then withVis else silenceVis) . dropLogs . evalDefStateT @Cache . evalIRBuilder' . evalPassManager'

runGraph :: (pass ~ TestPass, MonadIO m, MonadFix m, PrimMonad m, Pass.KnownDescription pass, Pass.PassInit pass (PassManager (IRBuilder (StateT Cache (Logger DropLogger (ExceptT' m))))))
              => SubPass pass (PMStack m) a -> m (Either SomeException a)
runGraph p = runPM False $ runRegs >> Pass.eval' p

silenceVis :: Monad m => Vis.VisStateT m a -> m a
silenceVis = fmap fst . Vis.newRunDiffT

withVis :: MonadIO m => Vis.VisStateT m a -> m a
withVis m = do
    (p, vis) <- Vis.newRunDiffT m
    let cfg = ByteString.unpack $ encode vis
    when (not . null $ vis ^. Vis.steps) $ do
        env <- liftIO $ lookupEnv "DEBUGVIS"
        case env of
            Just path -> liftIO $ do
                uid <- UUID.toString <$> UUID.nextRandom
                writeFile (path ++ "/" ++ uid ++ ".json") cfg
                void $ openBrowser $ "http://localhost:8000?cfgPath=" <> uid
            _ -> return ()
    return p
