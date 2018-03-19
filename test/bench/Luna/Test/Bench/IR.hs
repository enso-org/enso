{-# LANGUAGE OverloadedStrings #-}

module Luna.Test.Bench.IR where

import Prologue

import qualified Control.Monad.Exception     as Exception
import qualified Control.Monad.State.Layered as State
import qualified Criterion.Main              as Criterion
import qualified Criterion.Measurement       as Criterion
import qualified Criterion.Types             as Criterion hiding (measure)
import qualified Data.List                   as List
import qualified Data.Tuple.Strict           as Tuple
import qualified Data.TypeMap.Strict         as TypeMap
import qualified Foreign.Marshal.Alloc       as Ptr
import qualified Foreign.Marshal.Utils       as Ptr
import qualified Foreign.Memory.Pool         as MemPool
import qualified Foreign.Storable            as Storable
import qualified Luna.IR                     as IR
import qualified Luna.IR.Format              as Format
import qualified Luna.IR.Layer               as Layer
import qualified Luna.Pass                   as Pass
import qualified Luna.Pass.Scheduler         as Scheduler
import qualified Luna.Runner                 as Runner
import qualified OCI.IR.Layer.Internal       as Layer
import qualified OCI.Pass.Definition         as Pass
import qualified OCI.Pass.Encoder            as Encoder
import qualified OCI.Pass.Registry           as Registry
import qualified System.Console.ANSI         as ANSI

import Control.DeepSeq   (force)
import Control.Exception (evaluate)
import Luna.Pass         (Pass)
import OCI.IR.Component  (Component (Component))



-----------------------------
-- === Benchmark utils === --
-----------------------------

data Bench = Bench
    { _name :: String
    , _func :: Int -> IO ()
    }
makeLenses ''Bench

(<+>) :: String -> String -> String
a <+> b = a <> " " <> b

quoted :: String -> String
quoted a = "'" <> a <> "'"


timeIt :: NFData a => IO a -> IO Double
timeIt act = do
    start <- Criterion.getTime
    out <- act
    evaluate . force $ out
    end <- Criterion.getTime
    pure $ end - start

timeItExp :: NFData a => Int -> (Int -> IO a) -> IO Double
timeItExp exp f = timeIt . f $ 10 ^ exp

checkToRef :: Int -> Double -> Bench -> Bench -> IO Bool
checkToRef exp percAllow ref f = do
    stime <- timeItExp exp $ ref ^. func
    btime <- timeItExp exp $ f   ^. func
    if (btime < stime)
        then return True
        else return $ ((btime - stime) / stime) * 100 < percAllow

checkRetry :: IO Bool -> IO (Either Int Int)
checkRetry = checkRetry' 1 where
    checkRetry' n f = if n > maxRetries
        then return $ Left maxRetries
        else f >>= \case
            True  -> return $ Right n
            False -> checkRetry' (n + 1) f

assertBenchToRef :: String -> Int -> Double -> Bench -> Bench -> IO Bool
assertBenchToRef str exp percAllow ref f = do
    let desc = str <+> (f ^. name) <+> "as fast as" <+> (ref ^. name)
        msg  = ("[TEST:" <> show maxRetries <> "]") <+> desc
    putStrLn msg
    checkRetry (checkToRef exp percAllow ref f) >>= \case
        Left n -> do
            ANSI.cursorUpLine 1
            printStatus "FAIL" n ANSI.Red
            return False
        Right n -> do
            ANSI.cursorUpLine 1
            printStatus "PASS" n ANSI.Green
            return True

printStatus :: String -> Int -> ANSI.Color -> IO ()
printStatus status n color = do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid color]
    putStrLn $ "[" <> status <> ":" <> show n <> "]"
    ANSI.setSGR [ANSI.Reset]

maxRetries :: Int
maxRetries = 9

checkInvariants :: [IO Bool] -> IO ()
checkInvariants invs = do
    putStrLn ""
    putStrLn $ "Testing invariants (max retries = " <> show maxRetries <> "):"
    putStrLn ""
    ok <- and <$> sequence invs
    when_ (not ok) $ do
        ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
        putStrLn $ "One or more invariants failed."
        putStrLn $ "Invariants may fail depending on current CPU usage."
        putStrLn $ "Try running them again. If the problem persist, it means"
        putStrLn $ "that the implementation is broken and need to be fixed."
        ANSI.setSGR [ANSI.Reset]
    putStrLn ""

loopRange :: Int
loopRange = 1

loop :: Int -> [Int]
loop i = [i .. (i + loopRange - 1)]

expNFIO :: Bench -> Int -> Criterion.Benchmark
expNFIO f i = Criterion.bench ("10e" <> show i)
            . Criterion.nfIO $ (f ^. func) (10 ^ i)

bench :: Int -> Bench -> Criterion.Benchmark
bench i f = Criterion.bgroup (f ^. name) $ expNFIO f <$> loop i



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



----------------------------------
-- === Read / Write Layers === --
----------------------------------

foreign import ccall unsafe "c_ptr_rwloop"
    c_ptr_rwloop :: Int -> Int -> IO Int

readWrite_cptr :: Bench
readWrite_cptr = Bench "cptr" $ void . c_ptr_rwloop 1 ; {-# INLINE readWrite_cptr #-}

readWrite_ptr :: Bench
readWrite_ptr = Bench "rawPtr" $ \i -> do
    !ptr <- Ptr.new (0 :: Int)
    let go !0 = return ()
        go !j = do
            !x <- Storable.peek ptr
            Storable.poke ptr $! x+1
            go $! j - 1
    go i
    Ptr.free ptr
{-# NOINLINE readWrite_ptr #-}

mockNewComponent :: MonadIO m => m (IR.Term Format.Draft)
mockNewComponent = Component . coerce <$> MemPool.allocPtr @(IR.UniTerm ()) ; {-# INLINE mockNewComponent #-}

readWrite_expTM :: Bench
readWrite_expTM = Bench "explicitTypeMap" $ \i -> do
    a <- mockNewComponent
    Layer.unsafeWriteByteOff @IR.Model layerLoc a (IR.UniTermVar $ IR.Var 0)
    let go 0 = pure ()
        go j = do
            !set <- State.get'
            let !off = TypeMap.getElem @Int set
            IR.UniTermVar (IR.Var !x) <- Layer.unsafeReadByteOff @IR.Model off a
            Layer.unsafeWriteByteOff @IR.Model off a
                $ IR.UniTermVar $ IR.Var $! x + 1
            go (j - 1)
    State.evalT (go i) s
    where layerLoc :: Int
          layerLoc = 0
          s :: TypeMap.TypeMap '[ Proxy 1, Proxy 2, Proxy 3, Proxy 4, Int]
          s = TypeMap.TypeMap $ Tuple.T5
              (Proxy :: Proxy 1)
              (Proxy :: Proxy 2)
              (Proxy :: Proxy 3)
              (Proxy :: Proxy 4)
              (0 :: Int)
{-# NOINLINE readWrite_expTM #-}

readWrite_layerMock :: Bench
readWrite_layerMock = Bench "staticRun" $ \i -> do
    ir <- mockNewComponent
    Layer.unsafeWriteByteOff @IR.Model layerLoc ir (IR.UniTermVar $ IR.Var 0)
    let go :: Int -> Pass Pass.BasicPass ()
        go 0 = pure ()
        go j = do
            IR.UniTermVar (IR.Var !x) <- Layer.read @IR.Model ir
            Layer.write @IR.Model ir (IR.UniTermVar $ IR.Var $! (x+1))
            go (j - 1)

    cfg   <- Registry.evalT localRegistry
    state <- Encoder.run @Pass.BasicPass cfg
    Pass.eval (go i) state
    where layerLoc :: Int
          layerLoc = 0
          localRegistry :: (Registry.Monad m, MonadIO m) => m Encoder.State
          localRegistry = do
               Runner.registerAll
               reg     <- State.get @Registry.State
               passCfg <- Encoder.computeConfig reg
               pure passCfg
{-# NOINLINE readWrite_layerMock #-}

readWrite_layer :: Bench
readWrite_layer = Bench "normal" $ \i -> runPass' $ do
    !a <- IR.var 0
    let go :: Int -> Pass Pass.BasicPass ()
        go 0 = pure ()
        go j = do
            IR.UniTermVar (IR.Var !x) <- Layer.read @IR.Model a
            Layer.write @IR.Model a (IR.UniTermVar $ IR.Var $! (x+1))
            go (j - 1)
    go i
{-# NOINLINE readWrite_layer #-}



-----------------------
-- === Create IR === --
-----------------------

createIR_mallocPtr :: Bench
createIR_mallocPtr = Bench "mallocPtr" $ \i -> do
    let go !0 = pure ()
        go !j = do
            !ptr <- Ptr.new (0 :: Int)
            go $! j - 1
    go i
{-# NOINLINE createIR_mallocPtr #-}

createIR_normal :: Bench
createIR_normal = Bench "normal" $ \i -> runPass' $ do
    let go !0 = pure ()
        go !j = do
            !ir <- IR.var 0
            go $! j - 1
    go i
{-# NOINLINE createIR_normal #-}


--------------------------------
-- === Running Benchmarks === --
--------------------------------

invariants :: IO ()
invariants = checkInvariants $
    [ assertBenchToRef "Create IR" 6 10 createIR_mallocPtr createIR_normal
    , assertBenchToRef "Layer R/W" 7 10 readWrite_cptr readWrite_layer
    ]

benchmarks :: IO ()
benchmarks = Criterion.defaultMain
    [ "ir"
        [ "layer"
            [ "rw" $ bench 7 <$>
                [ readWrite_cptr
                , readWrite_ptr
                , readWrite_expTM
                , readWrite_layerMock
                , readWrite_layer
                ]
            ]

        , "create" $ bench 6 <$>
            [ createIR_mallocPtr
            , createIR_normal
            ]
        ]
    ]

main :: IO ()
main = do
    invariants
    benchmarks


instance a ~ Criterion.Benchmark
      => IsString ([Criterion.Benchmark] -> a) where
    fromString = Criterion.bgroup
