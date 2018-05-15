{-# LANGUAGE UndecidableInstances #-}

module Luna.Runner where

import Prologue hiding (Type)

import qualified Luna.IR                       as IR
import qualified OCI.Pass.Management.Registry  as Registry
import qualified OCI.Pass.Management.Scheduler as Scheduler
import qualified OCI.Pass.State.Encoder        as Pass.Encoder

import Control.Monad.Exception       (Throws)
import Data.Graph.Class              (Graph)
import OCI.Pass.Management.Scheduler (SchedulerT)


registerAll :: Registry.Monad m => m ()
registerAll = do
    Registry.registerComponent @IR.Terms
    Registry.registerPrimLayer @IR.Terms @IR.Model
    -- Registry.registerPrimLayer @IR.Terms @IR.Type
    -- Registry.registerPrimLayer @IR.Terms @IR.Users

    Registry.registerComponent @IR.Links
    Registry.registerPrimLayer @IR.Links @IR.Source
    Registry.registerPrimLayer @IR.Links @IR.Target


runManual :: (MonadIO m, Throws '[Registry.Error, Pass.Encoder.Error] m)
          => SchedulerT m a -> m a
runManual = Scheduler.runManual registerAll ; {-# INLINE runManual #-}
