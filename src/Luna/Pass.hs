{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass where

import Prologue

import qualified Control.Monad.Exception     as Exception
import qualified Control.Monad.State.Layered as State
import qualified OCI.Pass.Cache              as Pass
import qualified OCI.Pass.Manager            as PassManager

import Luna.IR.Link     (Links, Source, Target)
import Luna.IR.Term     (Model, Terms)
import OCI.Pass.Class   as Pass
import OCI.Pass.Manager (MonadPassManager)

import ToRefactor ()




type family BasicSpec t where
    BasicSpec (In Elems) = '[Terms, Links]
    BasicSpec (In Terms) = '[Model, Type]
    BasicSpec (In Links) = '[Source, Target]
    BasicSpec (Out a)    = BasicSpec (In a)
    BasicSpec t          = '[]



test_pm_run :: MonadIO m => m Pass.PassConfig
test_pm_run = Exception.catchAll undefined $ PassManager.evalT test_pm

test_pm :: (MonadPassManager m, MonadIO m) => m Pass.PassConfig
test_pm = do
    PassManager.registerComponent @Terms
    PassManager.registerPrimLayer @Terms @Model
    PassManager.registerPrimLayer @Terms @Type

    PassManager.registerComponent @Links
    PassManager.registerPrimLayer @Links @Source
    PassManager.registerPrimLayer @Links @Target

    reg <- State.get @PassManager.Registry
    passCfg <- PassManager.mkPassConfig reg

    pure passCfg


-- passTest :: Pass.Pass BasicPass
-- passTest = do
--     v1 <- var 5
--     v2 <- var 7
--     v3 <- var 9
--     l1 <- Link.new v1 v2

--     Layer.write @Type v1 l1

--     s <- Layer.read @Source l1
--     m <- Layer.read @Model s
--     print m
--     pure ()

-- passTest_run :: IO ()
-- passTest_run = do

--     cfg <- test_pm_run
--     xx <- Pass.encodePassState cfg
--     Pass.runPass xx passTest
