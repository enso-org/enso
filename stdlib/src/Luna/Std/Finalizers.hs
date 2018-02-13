module Luna.Std.Finalizers where

import Luna.Prelude
import Data.Map                (Map)
import Control.Concurrent      (newMVar, modifyMVar_, MVar)

import           Data.UUID     (UUID)
import qualified Data.UUID.V4  as UUID

newtype Finalizers = Finalizers (Map UUID (IO ()))
makeWrapped ''Finalizers

instance Default Finalizers where
    def = Finalizers def

runFinalizers :: Finalizers -> IO Finalizers
runFinalizers st = sequence (unwrap st) >> return def

newtype FinalizersCtx = FinalizersCtx (MVar Finalizers)
makeWrapped ''FinalizersCtx

registerFinalizer :: FinalizersCtx -> IO () -> IO UUID
registerFinalizer ctx finalizer = do
    uuid <- UUID.nextRandom
    modifyMVar_ (unwrap ctx) $ return . (wrapped . at uuid ?~ finalizer)
    return uuid

cancelFinalizer :: FinalizersCtx -> UUID -> IO ()
cancelFinalizer ctx uuid = modifyMVar_ (unwrap ctx) $ return . (wrapped . at uuid .~ Nothing)

finalize :: FinalizersCtx -> IO ()
finalize ctx = modifyMVar_ (unwrap ctx) runFinalizers

initFinalizersCtx :: IO FinalizersCtx
initFinalizersCtx = FinalizersCtx <$> newMVar def
