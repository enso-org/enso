module Luna.Std.Finalizers where

import Prologue
import Data.Map                (Map)
import Control.Concurrent      (MVar, newMVar, modifyMVar_, swapMVar)

import           Data.UUID     (UUID)
import qualified Data.UUID.V4  as UUID



------------------------
-- === Finalizers === --
------------------------

-- === Definition === --

newtype Finalizers = Finalizers (Map UUID (IO ()))
makeLenses ''Finalizers


-- === API === --

runFinalizers :: Finalizers -> IO Finalizers
runFinalizers st = sequence (unwrap st) >> pure def


-- === Instances === --

instance Default Finalizers where
    def = Finalizers def



---------------------------
-- === FinalizersCtx === --
---------------------------

-- === Definition === --

newtype FinalizersCtx = FinalizersCtx (MVar Finalizers)
makeLenses ''FinalizersCtx


-- === API === --

registerFinalizer :: FinalizersCtx -> IO () -> IO UUID
registerFinalizer ctx finalizer = do
    uuid <- UUID.nextRandom
    modifyMVar_ (unwrap ctx) $ pure . (wrapped . at uuid .~ Just finalizer)
    pure uuid

cancelFinalizer :: FinalizersCtx -> UUID -> IO ()
cancelFinalizer ctx uuid = modifyMVar_ (unwrap ctx)
    $ pure . (wrapped . at uuid .~ Nothing)

finalize :: FinalizersCtx -> IO ()
finalize ctx = do
    map <- swapMVar (unwrap ctx) def
    void $ runFinalizers map

initFinalizersCtx :: IO FinalizersCtx
initFinalizersCtx = FinalizersCtx <$> newMVar def

