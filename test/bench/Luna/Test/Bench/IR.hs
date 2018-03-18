module Luna.Test.Bench.IR where

import Prologue

import qualified Criterion.Main as Criterion
import qualified ToRefactor     as Basic

import qualified Control.Monad.Exception     as Exception
import qualified Control.Monad.State.Layered as State
import qualified Foreign.Marshal.Alloc       as Ptr
import qualified Foreign.Marshal.Utils       as Ptr
import qualified Foreign.Storable            as Storable
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
runPass !pass = Runner.runManual $ do
    Scheduler.registerPassFromFunction__ pass
    Scheduler.runPassSameThreadByType @pass
{-# INLINE runPass #-}

runPass' :: Pass Pass.BasicPass () -> IO ()
runPass' = runPass
{-# INLINE runPass' #-}





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


readWrite_Ptr :: Int -> IO ()
readWrite_Ptr i = do
    !ptr <- Ptr.new (0 :: Int)
    let go !0 = return ()
        go !j = do
            !x <- Storable.peek ptr
            Storable.poke ptr $! x+1
            go $! j - 1
    go i
    Ptr.free ptr
{-# NOINLINE readWrite_Ptr #-}

readWrite_Layer :: Int -> IO ()
readWrite_Layer i = runPass' $ do
    !a <- IR.var 0
    let go :: Int -> Pass Pass.BasicPass ()
        go 0 = pure ()
        go j = do
            IR.UniTermVar (IR.Var !x) <- Layer.read @IR.Model a
            Layer.write @IR.Model a (IR.UniTermVar $ IR.Var $! (x+1))
            go (j - 1)
    go i
{-# NOINLINE readWrite_Layer #-}


main :: IO ()
main = Criterion.defaultMain
    [ Criterion.bgroup "Read/Write"
        [ bench 7 "Raw Ptr"  readWrite_Ptr
        , bench 7 "IR Layer" readWrite_Layer
        ]
    ]
