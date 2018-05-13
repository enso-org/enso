{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Luna.Test.Bench.IR where

import Prologue

import qualified Control.Monad.Exception               as Exception
import qualified Control.Monad.State.Layered           as State
import qualified Criterion.Main                        as Criterion
import qualified Criterion.Measurement                 as Criterion
import qualified Data.Graph.Class                      as Graph
import qualified Data.Graph.Component.Node.Destruction as IR
import qualified Data.Graph.Data.Component.Class       as Component
import qualified Data.Graph.Data.Layer.Class           as Layer
import qualified Data.Graph.Data.Layer.Layout          as Layout
import qualified Data.Graph.Traversal.Discovery        as Discovery
import qualified Data.Set                              as Set
import qualified Data.Tuple.Strict                     as Tuple
import qualified Data.TypeMap.Strict                   as TypeMap
import qualified Foreign.Marshal.Alloc                 as Ptr
import qualified Foreign.Marshal.Utils                 as Ptr
import qualified Foreign.Memory.Pool                   as MemPool
import qualified Foreign.Storable                      as Storable
import qualified Luna.IR                               as IR
import qualified Luna.IR.Term.Format                   as Format
import qualified Luna.Pass                             as Pass
import qualified Luna.Pass.Scheduler                   as Scheduler
import qualified Luna.Runner                           as Runner
import qualified OCI.Pass.Definition.Class             as Pass
import qualified OCI.Pass.Management.Registry          as Registry
import qualified OCI.Pass.State.Encoder                as Encoder
import qualified OCI.Pass.State.IRInfo                 as IRInfo
import qualified OCI.Pass.State.Runtime                as Runtime
import qualified System.Console.ANSI                   as ANSI

import Control.DeepSeq   (force)
import Control.Exception (evaluate)
import Data.Graph.Data   (Component (Component))
import Data.Set          (Set)
import Luna.Pass         (Pass)



---------------------
-- === Orphans === --
---------------------

instance a ~ Criterion.Benchmark
      => IsString ([Criterion.Benchmark] -> a) where
    fromString = Criterion.bgroup



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
    void . evaluate . force $ out
    end <- Criterion.getTime
    pure $ end - start

timeItExp :: NFData a => Int -> (Int -> IO a) -> IO Double
timeItExp exp f = timeIt . f $ 10 ^ exp

checkToRef :: Int -> Double -> Bench -> Bench -> IO Bool
checkToRef exp percAllow ref f = do
    stime <- timeItExp exp $ ref ^. func
    btime <- timeItExp exp $ f   ^. func
    if (btime < stime)
        then pure True
        else pure $ ((btime - stime) / stime) * 100 < percAllow

checkRetry :: IO Bool -> IO (Either Int Int)
checkRetry = checkRetry' 1 where
    checkRetry' n f = if n > maxRetries
        then pure $ Left maxRetries
        else f >>= \case
            True  -> pure $ Right n
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
            pure False
        Right n -> do
            ANSI.cursorUpLine 1
            printStatus "PASS" n ANSI.Green
            pure True

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

type OnDemandPass pass m = (Typeable pass, Pass.Compile pass IO, MonadIO m
    , Exception.MonadException Registry.Error m
    , Exception.MonadException Scheduler.Error m
    , Exception.MonadException Encoder.Error m
    )

runPass :: ∀ pass m. OnDemandPass pass m => Pass pass IO () -> m ()
runPass !pass = Runner.runManual $ do
    Scheduler.registerPassFromFunction__ pass
    Scheduler.runPassSameThreadByType @pass
{-# INLINE runPass #-}

runPass' :: OnDemandPass Pass.BasicPass m => Pass Pass.BasicPass IO () -> m ()
runPass' = runPass
{-# INLINE runPass' #-}



----------------------------------
-- === Read / Write Layers === --
----------------------------------

foreign import ccall unsafe "c_ptr_rwloop"
    c_ptr_rwloop :: Int -> Int -> IO Int

readWrite_cptr :: Bench
readWrite_cptr = Bench "cptr" $ void . c_ptr_rwloop 1
{-# INLINE readWrite_cptr #-}

readWrite_ptr :: Bench
readWrite_ptr = Bench "rawPtr" $ \i -> do
    !ptr <- Ptr.new (0 :: Int)
    let go !0 = pure ()
        go !j = do
            !x <- Storable.peek ptr
            Storable.poke ptr $! x + 1
            go $! j - 1
    go i
    Ptr.free ptr
{-# NOINLINE readWrite_ptr #-}

mockNewComponent :: MonadIO m => m (IR.Term Format.Draft)
mockNewComponent = Component . coerce <$> MemPool.allocPtr @(IR.UniTerm ())
{-# INLINE mockNewComponent #-}

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
    let go :: Int -> Pass Pass.BasicPass IO ()
        go 0 = pure ()
        go j = do
            IR.UniTermVar (IR.Var !x) <- Layer.read @IR.Model ir
            Layer.write @IR.Model ir (IR.UniTermVar $ IR.Var $! x + 1)
            go (j - 1)

    cfg   <- Registry.evalT localRegistry
    state <- Encoder.run @Pass.BasicPass cfg
    Pass.eval (go i) state
    where layerLoc :: Int
          layerLoc = 0
          localRegistry :: (Registry.Monad m, MonadIO m)
                        => m IRInfo.CompiledIRInfo
          localRegistry = do
               Runner.registerAll
               reg     <- State.get @IRInfo.IRInfo
               passCfg <- IRInfo.compile reg
               pure passCfg
{-# NOINLINE readWrite_layerMock #-}

readWrite_layer :: Bench
readWrite_layer = Bench "normal" $ \i -> runPass' $ do
    !a <- IR.var 0
    let go :: Int -> Pass Pass.BasicPass IO ()
        go 0 = pure ()
        go j = do
            IR.UniTermVar (IR.Var !x) <- Layer.read @IR.Model a
            Layer.write @IR.Model a (IR.UniTermVar $ IR.Var $! x + 1)
            go (j - 1)
    go i
{-# NOINLINE readWrite_layer #-}

readWrite_layerptr :: Bench
readWrite_layerptr = Bench "normal" $ \i -> runPass' $ do
    !a <- IR.var 0
    let go :: Int -> Pass Pass.BasicPass IO ()
        go 0 = pure ()
        go j = do
            !tp <- Layer.read @IR.Type a
            !s  <- Layer.read @IR.Source tp
            -- Layer.write @IR.Model a (IR.UniTermVar $ IR.Var $! x + 1)
            go (j - 1)
    go i
{-# NOINLINE readWrite_layerptr #-}



-----------------------
-- === Create IR === --
-----------------------

createIR_mallocPtr :: Bench
createIR_mallocPtr = Bench "mallocPtr" $ \i -> do
    let go !0 = pure ()
        go !j = do
            !_ <- Ptr.new (0 :: Int)
            go $! j - 1
    go i
{-# NOINLINE createIR_mallocPtr #-}

createIR_normal :: Bench
createIR_normal = Bench "normal" $ \i -> runPass' $ do
    let go !0 = pure ()
        go !j = do
            !_ <- IR.var 0
            go $! j - 1
    go i
{-# NOINLINE createIR_normal #-}

createIR_normal2 :: Bench
createIR_normal2 = Bench "normal2" $ \i -> runPass' $ do
    let go !0 = pure ()
        go !j = do
            !v <- IR.var 0
            IR.delete v
            go $! j - 1
    go i
{-# NOINLINE createIR_normal2 #-}

createIR_normal3 :: Bench
createIR_normal3 = Bench "normal3" $ \i -> runPass' $ do
    let go !0 = pure ()
        go !j = do
            !v <- IR.var 0
            !tpLink <- Layer.read @IR.Type v
            -- Component.destruct tpLink
            Component.destruct v
            go $! j - 1
    go i
{-# NOINLINE createIR_normal3 #-}

class Foo a where foo :: IO ()
instance Foo a where foo = pure () ; {-# INLINE foo #-}



data Luna
type instance Graph.Components      Luna          = '[IR.Terms, IR.Links]
type instance Graph.ComponentLayers Luna IR.Terms = '[IR.Model, IR.Users, IR.Type] -- , IR.Users]
type instance Graph.ComponentLayers Luna IR.Links = '[IR.Source, IR.Target]

type instance Graph.DiscoverGraphTag m = Luna -- HACK

createIR_normal4 :: Bench
createIR_normal4 = Bench "normal4" $ \i -> runPass' $ do
    let go !0 = pure ()
        go !j = do
            !v <- IR.var 0
            !tpLink <- Layer.read @IR.Type v
            -- Component.destruct tpLink
            -- Component.destruct v
            -- modelManager = Layer.manager @IR.Model
            -- typeManager  = Layer.manager @IR.Type
            liftIO $ foo @IR.Model
            liftIO $ foo @IR.Type

            go $! j - 1
    go i
{-# NOINLINE createIR_normal4 #-}


--------------------------
-- === IR Discovery === --
--------------------------

-- discoverIR_simple :: Bench
-- discoverIR_simple = Bench "simple" $ \i -> Graph.run @Luna $ runPass' $ do
--     v <- IR.var "a"
--     let go !0 = pure ()
--         go !j = do
--             !x <- Discovery.discover v
--             go $! j - 1
--     go i
-- {-# NOINLINE discoverIR_simple #-}

discoverIR_hack :: Bench
discoverIR_hack = Bench "manual" $ \i -> Graph.run @Luna $ runPass' $ do
    v <- IR.var "a"
    let go !0 = pure ()
        go !j = do
            let !f = manualDiscoverIRHack (Layout.relayout v)
            !out <- f []
            go $! j - 1
    go i
{-# NOINLINE discoverIR_hack #-}

-- | The manualDiscoverIRHack function allows us to discover IR with an
--   assumption that everything is just a Var with chain of Top types.
manualDiscoverIRHack
    :: ( Layer.Reader IR.Term IR.Type   m
       , Layer.Reader IR.Term IR.Model  m
       , Layer.Reader IR.Link IR.Source m
       , Layer.Reader IR.Link IR.Target m
       , MonadIO m
       )
    => IR.SomeTerm -> [Component.Any] -> m [Component.Any]
manualDiscoverIRHack !term !r = do
    !tl    <- Layer.read @IR.Type  term
    !model <- Layer.read @IR.Model term
    let !r' = case model of
            IR.UniTermVar (IR.Var {}) -> let !o = Layout.relayout term : r in o
            IR.UniTermTop (IR.Top {}) -> let !o = Layout.relayout term : r in o
            a                         -> error "not supported"
    !src <- Layer.read @IR.Source tl
    !tgt <- Layer.read @IR.Target tl
    let !(src' :: IR.SomeTerm) = Layout.relayout src
    let !(tgt' :: IR.SomeTerm) = Layout.relayout tgt
    let !f = if src' == tgt' then pure else manualDiscoverIRHack src'
        !r'' = Layout.relayout tl : r'
        !mr''' = f r''
    mr'''


discoverIR_hack3 :: Bench
discoverIR_hack3 = Bench "generic" $ \i -> Graph.run @Luna $ runPass' $ do
    !v <- IR.var "a"
    -- print "!!!"
    -- print =<< State.get @(Runtime.LayerByteOffset IR.Terms IR.Model)
    -- print =<< State.get @(Runtime.LayerByteOffset IR.Terms IR.Type)
    -- print =<< State.get @(Runtime.LayerByteOffset IR.Terms IR.Users)
    let go !0 = let !o = pure () in o
        go !j = do
            -- putStrLn ""
            !out <- Discovery.getNeighbours v
            -- print out
            -- !tl  <- Layer.read @IR.Type v
            -- !t   <- Layer.read @IR.Source tl
            -- !ttl <- Layer.read @IR.Type t
            -- print [Component.unsafeToPtr v, Component.unsafeToPtr tl, Component.unsafeToPtr t, Component.unsafeToPtr ttl]
            let !o = go $! j - 1 in o
    go i
{-# NOINLINE discoverIR_hack3 #-}




--------------------------------
-- === Running Benchmarks === --
--------------------------------

-- test :: Bench
-- test = Bench "test" $ \i -> do
--     let go !0 = pure ()
--         go !j = do
--             (s :: PtrSet.UnmanagedPtrSet ()) <- PtrSet.new
--             go $! j - 1
--     go i
-- {-# NOINLINE test #-}


invariants :: IO ()
invariants = checkInvariants $
    -- [ assertBenchToRef "Create IR" 6 10 createIR_mallocPtr createIR_normal
    [ assertBenchToRef "Layer R/W" 7 10 readWrite_cptr readWrite_layer
    ]

benchmarks :: IO ()
benchmarks = do
    Criterion.defaultMain
      [ "ir"
        [ "layer"
            [ "rw" $ bench 7 <$>
                [ readWrite_cptr
                , readWrite_ptr
                , readWrite_expTM
                -- , readWrite_layerMock
                , readWrite_layer
                ]
            ]

        , "create" $ bench 5 <$>
            [ createIR_mallocPtr
            , createIR_normal
            , createIR_normal2
            , createIR_normal3
            , createIR_normal4
            ]
        -- [ "layer" $ bench 7 <$>
        --     [readWrite_layerptr]

        , "discovery" $ bench 6 <$>
            [ discoverIR_hack3
            , discoverIR_hack
            -- , discoverIR_simple
            ]
        ]
      ]

main :: IO ()
main = do
    invariants
    benchmarks



