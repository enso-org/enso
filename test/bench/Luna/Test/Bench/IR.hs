module Luna.Test.Bench.IR where

import Prologue

import qualified Criterion.Main as Criterion
import qualified ToRefactor     as Basic

import qualified Control.Monad.Exception     as Exception
import qualified Control.Monad.State.Layered as State
import qualified Luna.IR                     as IR
import qualified Luna.IR.Layer               as Layer
import qualified Luna.Pass                   as Pass
import qualified Luna.Pass.Scheduler         as Scheduler
import qualified Luna.Runner                 as Runner
import qualified OCI.IR.Layer.Internal       as Layer
import qualified OCI.Pass.Definition         as Pass
import qualified OCI.Pass.Encoder            as Encoder
import qualified OCI.Pass.Registry           as Registry

import Luna.Pass (Pass)



-----------------------------
-- === Benchmark utils === --
-----------------------------

loopRange :: Int
loopRange = 1

loop :: Int -> [Int]
loop i = [i .. (i + loopRange - 1)]

expNFIO :: NFData a => (Int -> IO a) -> Int -> Criterion.Benchmark
expNFIO f i = Criterion.bench ("10e" <> show i) (Criterion.nfIO $ f (10 ^ i))

bench :: NFData a => Int -> String -> (Int -> IO a) -> Criterion.Benchmark
bench i s f = Criterion.bgroup s $ expNFIO f <$> loop i



----------------------------
-- === Benchark pass === --
----------------------------

type OnDemandPass pass = (Typeable pass, Pass.Compile pass IO)

runPass :: ∀ pass. OnDemandPass pass => Pass pass () -> IO ()
runPass pass = Runner.runManual $ do
    Scheduler.registerPassFromFunction__ pass
    Scheduler.runPassSameThreadByType @pass
{-# INLINE runPass #-}

runPass' :: Pass Pass.BasicPass () -> IO ()
runPass' = runPass ; {-# INLINE runPass' #-}


test1 :: Int -> IO ()
test1 i = runPass' $ do
    a <- Basic.mockNewComponent
    Layer.unsafeWriteByteOff @IR.Model Basic.layerLoc0 a (IR.UniTermVar $ IR.Var 0)
    let go :: Int -> Pass Pass.BasicPass ()
        go 0 = pure ()
        go j = do
            IR.UniTermVar (IR.Var !x) <- Layer.read @IR.Model a
            Layer.write @IR.Model a (IR.UniTermVar $ IR.Var $! (x+1))
            go (j - 1)
    go i
    return ()
{-# INLINE test1 #-}


test_readWriteLayer4 :: Int -> IO ()
test_readWriteLayer4 i = runPass' $ do
    ir <- Basic.mockNewComponent
    Layer.unsafeWriteByteOff @IR.Model Basic.layerLoc0 ir (IR.UniTermVar $ IR.Var 0)
    let go :: Int -> Pass Pass.BasicPass ()
        go 0 = pure ()
        go j = do
            IR.UniTermVar (IR.Var !x) <- Layer.read @IR.Model ir
            Layer.write @IR.Model ir (IR.UniTermVar $ IR.Var $! (x+1))
            go (j - 1)

    cfg <- liftIO $ test_pm_run
    xx  <- liftIO $ Encoder.run @Pass.BasicPass cfg
    -- liftIO $ Pass.eval (go i) xx
    go i


test_pm_run :: IO Encoder.State
test_pm_run = Registry.evalT test_pm

test_pm :: (Registry.Monad m, MonadIO m) => m Encoder.State
test_pm = do
    Runner.registerAll

    reg <- State.get @Registry.State
    passCfg <- Encoder.computeConfig reg

    pure passCfg



main :: IO ()
main = Criterion.defaultMain
    [ bench 8 "Read/Write Layer + State config 4 xx" test_readWriteLayer4
    , bench 8 "Read/Write Layer + State config 4 xx" test1
    ]
