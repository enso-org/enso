{-# LANGUAGE CPP               #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE UnboxedTuples     #-}
{-# LANGUAGE Unsafe            #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Luna.Test.Bench.IR where

import Prologue

import qualified Control.Monad.Exception               as Exception
import qualified Control.Monad.State.Layered           as State
import qualified Criterion.Main                        as Criterion
import qualified Criterion.Measurement                 as Criterion
import qualified Data.Graph.Data.Component.Class       as Component
import qualified Data.Graph.Data.Graph.Class           as Graph
import qualified Data.Graph.Data.Layer.Class           as Layer
import qualified Data.Graph.Data.Layer.Layout          as Layout
import qualified Data.Graph.Fold.Partition             as Partition
import qualified Data.Graph.Fold.SubComponents         as Traversal
import qualified Data.Graph.Fold.SubTree               as Traversal
import qualified Data.IORef                            as IORef
import qualified Data.Struct                           as Struct
import qualified Data.Tuple.Strict                     as Tuple
import qualified Data.TypeMap.MultiState               as MultiState
import qualified Data.TypeMap.Strict                   as TypeMap
import qualified Foreign.Marshal.Alloc                 as Ptr
import qualified Foreign.Marshal.Utils                 as Ptr
import qualified Foreign.Memory.Pool                   as MemPool
import qualified Foreign.Storable                      as Storable
import qualified Luna.IR                               as IR
import qualified Luna.IR.Term.Format                   as Format
import qualified Luna.Pass                             as Pass
import qualified Luna.Pass.Scheduler                   as Scheduler
import qualified System.Console.ANSI                   as ANSI

import Control.Concurrent              (threadDelay)
import Control.DeepSeq                 (force)
import Control.Exception               (evaluate)
import Control.Monad.Primitive         (primitive)
import Criterion.Main                  (Benchmark)
import Data.Graph.Data                 (Component (Component))
import GHC.Exts                        (Any, Int (I#), SmallMutableArray#,
                                        State#, readSmallArray#, unsafeCoerce#,
                                        writeSmallArray#)
import Luna.Pass                       (Pass)
import Luna.Pass.Basic                 (Compilation)


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
            False -> do
                printStatus "TEST" n [ANSI.Reset]
                threadDelay 1000000
                checkRetry' (n + 1) f

assertBenchToRef :: String -> Int -> Double -> Bench -> Bench -> IO Bool
assertBenchToRef str exp percAllow ref f = do
    let desc = str <+> (f ^. name) <+> "as fast as" <+> (ref ^. name)
        msg  = ("[TEST:" <> show maxRetries <> "]") <+> desc
    putStrLn msg
    checkRetry (checkToRef exp percAllow ref f) >>= \case
        Left n -> do
            printStatus "FAIL" n [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
            pure False
        Right n -> do
            printStatus "PASS" n [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
            pure True

printStatus :: String -> Int -> [ANSI.SGR] -> IO ()
printStatus status n color = do
    ANSI.cursorUpLine 1
    ANSI.setSGR color
    putStrLn $ "[" <> status <> ":" <> show n <> "]"
    ANSI.setSGR [ANSI.Reset]

maxRetries :: Int
maxRetries = 9

checkInvariants :: Double -> [IO Bool] -> IO ()
checkInvariants percAllow invs = do
    putStrLn ""
    putStrLn $ "Testing invariants (max retries = " <> show maxRetries <> ", max diff = " <> show percAllow <> "%):"
    putStrLn ""
    ok <- and <$> sequence invs
    when_ (not ok) $ do
        putStrLn ""
        printErrLn $ "One or more invariants failed."
        printErrLn $ "Invariants may fail depending on current CPU usage."
        printErrLn $ "Try running them again. If the problem persist, it means"
        printErrLn $ "that the implementation is broken and needs to be fixed."
        ANSI.setSGR [ANSI.Reset]
    putStrLn ""
    where printErrLn s = do
              ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
              putStrLn s

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

type OnDemandPass stage pass m =
    ( MonadIO m
    , Typeable pass
    , Pass.Compile stage pass m
    , Exception.MonadException Scheduler.Error m
    )

runPass :: ∀ stage pass m. OnDemandPass stage pass m
        => Pass stage pass () -> m ()
runPass !pass = Scheduler.evalT $ do
    Scheduler.registerPassFromFunction__ pass
    Scheduler.runPassSameThreadByType @pass
{-# INLINE runPass #-}

runPass' :: Pass Compilation Pass.BasicPass () -> IO ()
runPass' p = Graph.encodeAndEval @Compilation (runPass p)
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

-- readWrite_layerMock :: Bench
-- readWrite_layerMock = Bench "staticRun" $ \i -> do
--     ir <- mockNewComponent
--     Layer.unsafeWriteByteOff @IR.Model layerLoc ir (IR.UniTermVar $ IR.Var 0)
--     let go :: Int -> Pass Pass.BasicPass IO ()
--         go 0 = pure ()
--         go j = do
--             IR.UniTermVar (IR.Var !x) <- Layer.read @IR.Model ir
--             Layer.write @IR.Model ir (IR.UniTermVar $ IR.Var $! x + 1)
--             go (j - 1)

--     cfg   <- Registry.evalT localRegistry
--     state <- Encoder.run @Pass.BasicPass cfg
--     Pass.eval (go i) state
--     where layerLoc :: Int
--           layerLoc = 0
--           localRegistry :: (Registry.Monad m, MonadIO m)
--                         => m IRInfo.CompiledIRInfo
--           localRegistry = do
--                Runner.registerAll
--                reg     <- State.get @IRInfo.IRInfo
--                passCfg <- IRInfo.compile reg
--                pure passCfg
-- {-# NOINLINE readWrite_layerMock #-}

readWrite_layer :: Bench
readWrite_layer = Bench "normal" $ \i -> runPass' $ do
    !a <- IR.var 0
    let go :: Int -> Pass Compilation Pass.BasicPass ()
        go 0 = pure ()
        go j = do
            IR.UniTermVar (IR.Var !x) <- Layer.read @IR.Model a
            Layer.write @IR.Model a (IR.UniTermVar $ IR.Var $! x + 1)
            go (j - 1)
    go i
{-# NOINLINE readWrite_layer #-}




benchEnv :: NFData env => String -> IO env -> (env -> IO ()) -> Benchmark
benchEnv name env f = Criterion.env env
    $ \ (~a) -> Criterion.bench name $ Criterion.whnfIO $ f a
{-# INLINE benchEnv #-}



-- readWrite_layerptr :: Bench
-- readWrite_layerptr = Bench "normal" $ \i -> runPass' $ do
--     !a <- IR.var 0
--     let go :: Int -> Pass Pass.BasicPass ()
--         go 0 = pure ()
--         go j = do
--             !tp <- Layer.read @IR.Type a
--             !s  <- Layer.read @IR.Source tp
--             -- Layer.write @IR.Model a (IR.UniTermVar $ IR.Var $! x + 1)
--             go (j - 1)
--     go i
-- {-# NOINLINE readWrite_layerptr #-}



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

-- createIR_normal2 :: Bench
-- createIR_normal2 = Bench "normal2" $ \i -> runPass' $ do
--     let go !0 = pure ()
--         go !j = do
--             !v <- IR.var 0
--             IR.delete v
--             go $! j - 1
--     go i
-- {-# NOINLINE createIR_normal2 #-}

createIR_normal3 :: Bench
createIR_normal3 = Bench "normal3" $ \i -> runPass' $ do
    let go !0 = pure ()
        go !j = do
            !v <- IR.var 0
            !_ <- Layer.read @IR.Type v
            -- Component.destruct tpLink
            Component.destruct v
            go $! j - 1
    go i
{-# NOINLINE createIR_normal3 #-}


data MockEdge = MockEdge (IORef.IORef MockIR) (IORef.IORef MockIR)
data MockModel = MockVar !Int
data MockIR = MockIR !MockModel (IORef.IORef MockEdge) -- (Set.Set (IORef.IORef MockEdge))

mockNewVar :: Int -> IO (IORef.IORef MockIR)
mockNewVar i = do
    tref   <- IORef.newIORef undefined
    ttedge <- IORef.newIORef (MockEdge tref tref)
    let !t = MockIR (MockVar 0) ttedge -- mempty
    IORef.writeIORef tref t

    xref  <- IORef.newIORef undefined
    xedge <- IORef.newIORef (MockEdge xref tref)
    let !x = MockIR (MockVar i) xedge -- mempty
    IORef.writeIORef xref x

    pure xref

create_IORef :: Bench
create_IORef = Bench "ioref" $ \i -> do
    let go !0 = pure ()
        go !j = do
            !_ <- mockNewVar 0
            go $! j - 1
    go i
{-# NOINLINE create_IORef #-}




    -- createIR_normal4 :: Bench
    -- createIR_normal4 = Bench "normal4" $ \i -> runPass' $ do
    --     let go !0 = pure ()
    --         go !j = do
    --             !v <- IR.var 0
    --             !tpLink <- Layer.read @IR.Type v
    --             -- Component.destruct tpLink
    --             -- Component.destruct v
    --             -- modelManager = Layer.manager @IR.Model
    --             -- typeManager  = Layer.manager @IR.Type
    --             liftIO $ foo @IR.Model
    --             liftIO $ foo @IR.Type

    --             go $! j - 1
    --     go i
    -- {-# NOINLINE createIR_normal4 #-}


--------------------------
-- === IR Discovery === --
--------------------------


-- TODO [Performance]
-- We need to analyze why the manualDiscoverIRHack runs MUCH slower when we use
-- the if branch. Without it it runs about 60 times faster. Of course then we
-- remove the recursion too, but the recursion for each test evaluates only a
-- single time. It seems that in fact this could be a problem. This function
-- might not optimize to tight loop and we pay for function evaluation.
subTreeDiscovery_manual :: Bench
subTreeDiscovery_manual = Bench "manual" $ \i -> runPass' $ do
    v <- IR.var "a"
    let !empty = ACLNil
    let go !0 = pure ()
        go !j = do
            let !f = manualDiscoverIRHack (Layout.relayout v)
            !_ <- f empty
            go $! j - 1
    go i
{-# NOINLINE subTreeDiscovery_manual #-}

data AnyComponentList
    = ACLCons !Component.Any !AnyComponentList
    | ACLNil
    deriving (Show, Eq)

-- | The manualDiscoverIRHack function allows us to discover IR with an
--   assumption that everything is just a Var with chain of Top types.
manualDiscoverIRHack
    :: ( Layer.Reader IR.Term IR.Type   m
       , Layer.Reader IR.Term IR.Model  m
       , Layer.Reader IR.Link IR.Source m
       , Layer.Reader IR.Link IR.Target m
       , MonadIO m
       )
    => IR.SomeTerm -> AnyComponentList -> m AnyComponentList
manualDiscoverIRHack !term !r = do
    !tl    <- Layer.read @IR.Type  term
    !model <- Layer.read @IR.Model term
    let !r' = case model of
            IR.UniTermVar (IR.Var {}) -> let !o = ACLCons (Layout.relayout term) r in o
            IR.UniTermTop (IR.Top {}) -> let !o = ACLCons (Layout.relayout term) r in o
            _                         -> error "not supported"
    !src <- Layer.read @IR.Source tl
    !tgt <- Layer.read @IR.Target tl
    let !(src' :: IR.SomeTerm) = Layout.relayout src
    let !(tgt' :: IR.SomeTerm) = Layout.relayout tgt
    let !r'' = ACLCons (Layout.relayout tl) r'
    if src' == tgt' then pure r'' else manualDiscoverIRHack src' r''
-- {-# INLINABLE manualDiscoverIRHack #-}


subTreeDiscovery :: Bench
subTreeDiscovery = Bench "generic" $ \i -> runPass' $ do
    !v <- IR.var "a"
    let go !0 = let !o = pure () in o
        go !j = do
            !_ <- Traversal.subTree' v
            go $! j - 1
    go i
{-# NOINLINE subTreeDiscovery #-}

linkDiscovery :: Bench
linkDiscovery = Bench "link" $ \i -> runPass' $ do
    !v <- IR.var "a"
    let go !0 = let !o = pure () in o
        go !j = do
            !_ <- Traversal.subComponents @IR.Links v
            -- putStrLn ""
            -- print out
            -- !tl  <- Layer.read @IR.Type v
            -- print tl
            -- !t   <- Layer.read @IR.Source tl
            -- !ttl <- Layer.read @IR.Type t
            -- print [Component.unsafeToPtr v, Component.unsafeToPtr tl, Component.unsafeToPtr t, Component.unsafeToPtr ttl]
            go $! j - 1
    go i
{-# NOINLINE linkDiscovery #-}


partitionsSingleVar :: Bench
partitionsSingleVar = Bench "partitions single var" $ \i -> runPass' $ do
    !v <- IR.var "a"
    let go !0 = let !o = pure () in o
        go !j = do
            !_ <- Partition.partition v
            go $! j - 1
    go i
{-# NOINLINE partitionsSingleVar #-}

partitionsUnify :: Bench
partitionsUnify = Bench "partitions unify" $ \i -> runPass' $ do
    !a <- IR.var "a"
    !b <- IR.var "b"
    !u <- IR.unify a b
    let go !0 = let !o = pure () in o
        go !j = do
            !_ <- Partition.partition u
            go $! j - 1
    go i
{-# NOINLINE partitionsUnify #-}

-- externalSizeDiscovery_1n :: Bench
-- externalSizeDiscovery_1n = Bench "external size 1n" $ \i -> runPass' $ do
--     !v <- IR.var "a"
--     let go !0 = let !o = pure () in o
--         go !j = do
--             !_ <- Size.discover v
--             go $! j - 1
--     go i
-- {-# NOINLINE externalSizeDiscovery_1n #-}

-- externalSizeDiscovery_2n2e :: Bench
-- externalSizeDiscovery_2n2e = Bench "external size 2n 2e" $ \i -> runPass' $ do
--     !v <- IR.var "a"
--     !clusters <- Partition.partition v
--     let go !0 = let !o = pure () in o
--         go !j = do
--             !_ <- Graph.clusterSize @('[Nodes, Edges]) clusters
--             go $! j - 1
--     go i
-- {-# NOINLINE externalSizeDiscovery_2n2e #-}

newtype X = X Int
newtype Y = Y Int

readWrite_MS_1 :: Bench
readWrite_MS_1 = Bench "R/W MultiState" $ \i -> do
    let go 0 = pure ()
        go j = do
            X !x <- State.get @X
            let !x' = x + 1
            State.put @X $ X x'
            go (j - 1)
    MultiState.evalT (go i) s
    where s :: TypeMap.TypeMap '[ Proxy 1, Proxy 2, Proxy 3, Proxy 4, X ]
          s = TypeMap.TypeMap $ Tuple.T5
              (Proxy :: Proxy 1)
              (Proxy :: Proxy 2)
              (Proxy :: Proxy 3)
              (Proxy :: Proxy 4)
              (X 0)
{-# NOINLINE readWrite_MS_1 #-}


readWrite_MS_2 :: Bench
readWrite_MS_2 = Bench "R/W MultiState" $ \i -> do
    let go 0 = pure ()
        go j = do
            X !x <- State.get @X
            let !x' = x + 1
            State.put @X $ X x'
            go (j - 1)
    MultiState.evalT (MultiState.evalT (go i) s2) s
    where s :: TypeMap.TypeMap '[ Proxy 1, Proxy 2, Proxy 3, Proxy 4, X ]
          s = TypeMap.TypeMap $ Tuple.T5
              (Proxy :: Proxy 1)
              (Proxy :: Proxy 2)
              (Proxy :: Proxy 3)
              (Proxy :: Proxy 4)
              (X 0)
          s2 :: TypeMap.TypeMap '[ Proxy 1, Proxy 2, Proxy 3, Proxy 4, Y ]
          s2 = TypeMap.TypeMap $ Tuple.T5
              (Proxy :: Proxy 1)
              (Proxy :: Proxy 2)
              (Proxy :: Proxy 3)
              (Proxy :: Proxy 4)
              (Y 0)
{-# NOINLINE readWrite_MS_2 #-}


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


getField :: (PrimMonad m, Struct.Struct x) => x (PrimState m) -> m Int
getField = \x -> primitive (go (Struct.destruct x)) where
    go :: (forall s. SmallMutableArray# s Any -> State# s -> (# State# s, Int #))
    go = (\m s -> unsafeCoerce# readSmallArray# m i s) ; {-# INLINE go #-}
    !(I# i) = 0
{-# INLINE getField #-}

setField :: (PrimMonad m, Struct.Struct x) => x (PrimState m) -> Int -> m ()
setField = \x y -> primitive_ (go (Struct.destruct x) y) where
    go :: (forall s. SmallMutableArray# s Any -> Int -> State# s -> State# s)
    go = \m a s -> unsafeCoerce# writeSmallArray# m i a s ; {-# INLINE go #-}
    !(I# i) = 0
{-# INLINE setField #-}

-- field :: Int {- ^ slot -} -> Field s a
-- field (I# i) = Field
--   (\m s -> unsafeCoerce# readSmallArray# m i s)
--   (\m a s -> unsafeCoerce# writeSmallArray# m i a s)
-- {-# INLINE field #-}

readWrite_structs_layer :: Bench
readWrite_structs_layer = Bench "structs" $ \i -> do
    !obj <- Struct.alloc 1 :: IO (Struct.Object (PrimState IO))
    let _ = Struct.field 0 :: Struct.Field Struct.Object Int
    setField obj 0
    let go :: Int -> IO ()
        go 0 = pure ()
        go j = do
            !x <- getField obj
            setField obj $! x + 1
            go (j - 1)
    go i
{-# NOINLINE readWrite_structs_layer #-}

readWrite_ioref :: Bench
readWrite_ioref = Bench "ioref" $ \i -> do
    !ref <- IORef.newIORef (1 :: Int)
    let go :: Int -> IO ()
        go 0 = pure ()
        go j = do
            !x <- IORef.readIORef ref
            IORef.writeIORef ref $! x + 1
            go (j - 1)
    go i
{-# NOINLINE readWrite_ioref #-}
-- >>> isNil (Nil :: Object (PrimState IO))
-- True
-- >>> o <- alloc 1 :: IO (Object (PrimState IO))
-- >>> isNil o

invariants :: IO ()
invariants = checkInvariants maxPercDiff $
    -- [ assertBenchToRef "Create IR" 6 10 createIR_mallocPtr createIR_normal
    [ assertBenchToRef "Layer R/W        "  7 maxPercDiff readWrite_cptr readWrite_layer
    -- , assertBenchToRef "SubTree Discovery"  6 maxPercDiff subTreeDiscovery_manual subTreeDiscovery
    , assertBenchToRef "SubTree Partitions" 6 maxPercDiff subTreeDiscovery partitionsSingleVar
    -- , assertBenchToRef "Size Discovery"     6 (maxPercDiff + 300) externalSizeDiscovery_1n externalSizeDiscovery_2n2e
    ]
    where maxPercDiff = 5

benchmarks :: IO ()
benchmarks = do
    Criterion.defaultMain
      [ "ir"
        [ "layer"
            [ "rw" $ bench 4 <$>
                [ readWrite_cptr
                , readWrite_layer
                ]
                -- , readWrite_ptr
                -- , readWrite_expTM
                -- -- , readWrite_layerMock
        --         [ readWrite_layer
        --         , readWrite_structs_layer
        --         , readWrite_ioref
        --         -- , readWrite_MS_1
        --         -- , readWrite_MS_2
        --         ]
            ]
        , "create" $ bench 5 <$>
            [ createIR_mallocPtr
            , createIR_normal
            -- , createIR_normal2
            , createIR_normal3
            , create_IORef
        --     , createIR_normal4
            ]
        -- [ "layer" $ bench 7 <$>
        --     [readWrite_layerptr]

        , "discovery" $ bench 6 <$>
            [ subTreeDiscovery_manual
            , subTreeDiscovery
            , linkDiscovery
            , partitionsSingleVar
            , partitionsUnify
            -- , externalSizeDiscovery_1n
            -- , externalSizeDiscovery_2n2e
            ]
        ]
      ]


test :: IO ()
test = runPass' $ do
    !v <- IR.var "a"
    !v2 <- IR.var "a"
    !u <- IR.match v [v2]
    -- _ <- Graph.dumpComponent v undefined
    -- size <- External.size u
    ins <- IR.inputs u
    print ins
    -- print size

main :: IO ()
main = do
    test
    invariants
    benchmarks

