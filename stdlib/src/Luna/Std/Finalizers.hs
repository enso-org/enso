module Luna.Std.Finalizers where

import Prologue
import Data.Map                (Map)
import Control.Concurrent      (MVar, newMVar, modifyMVar_, putMVar, swapMVar)

import           Data.UUID     (UUID)
import qualified Data.UUID.V4  as UUID

newtype Finalizers = Finalizers (Map UUID (IO ()))
makeLenses ''Finalizers

instance Default Finalizers where
    def = Finalizers def

runFinalizers :: Finalizers -> IO Finalizers
runFinalizers st = sequence (unwrap st) >> return def

newtype FinalizersCtx = FinalizersCtx (MVar Finalizers)
makeLenses ''FinalizersCtx

registerFinalizer :: FinalizersCtx -> IO () -> IO UUID
registerFinalizer ctx finalizer = do
    uuid <- UUID.nextRandom
    modifyMVar_ (unwrap ctx) $ return . (wrapped . at uuid .~ Just finalizer)
    return uuid

cancelFinalizer :: FinalizersCtx -> UUID -> IO ()
cancelFinalizer ctx uuid = modifyMVar_ (unwrap ctx) $ return . (wrapped . at uuid .~ Nothing)

finalize :: FinalizersCtx -> IO ()
finalize ctx = do
    map <- swapMVar (unwrap ctx) def
    void $ runFinalizers map

initFinalizersCtx :: IO FinalizersCtx
initFinalizersCtx = FinalizersCtx <$> newMVar def
