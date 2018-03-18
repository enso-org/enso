{-# LANGUAGE UndecidableInstances #-}

module Luna.Runner where

import Prologue

import Luna.Pass

import qualified Luna.IR.Link       as Link
import qualified OCI.Pass.Cache     as Pass
import qualified OCI.Pass.Encoder   as Pass.Encoder
import qualified OCI.Pass.Registry  as Registry
import qualified OCI.Pass.Scheduler as Scheduler

import Luna.IR

import Control.Monad.Exception (Throws)
import OCI.Pass.Scheduler      (SchedulerT)


registerAll :: Registry.Monad m => m ()
registerAll = do
    Registry.registerComponent @Terms
    Registry.registerPrimLayer @Terms @Model
    -- Registry.registerPrimLayer @Terms @Type

    Registry.registerComponent @Links
    Registry.registerPrimLayer @Links @Source
    Registry.registerPrimLayer @Links @Target


runManual :: (MonadIO m, Throws '[Registry.Error, Pass.Encoder.Error] m)
          => SchedulerT m a -> m a
runManual = Scheduler.runManual registerAll
