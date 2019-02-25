{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import Prologue

-- import qualified Data.Storable                   as Data
-- import qualified Data.VectorSet.Mutable.Storable as VectorSet
import qualified Luna.Pass                       as Pass
-- import qualified Luna.Pass.Scheduler             as Scheduler
import qualified Luna.Test.Bench.IR              as IRBench
-- import qualified Luna.Test.Bench.Test            as Test

-- import Criterion.Measurement (initializeTime)
-- import Luna.Pass             (Pass)
import System.IO             (BufferMode (NoBuffering), hSetBuffering, stdout)


-----------------------
-- === Test pass === --
-----------------------

-- === Definition === --

data TestPass
type instance Pass.Spec TestPass t = TestPassSpec t
type family   TestPassSpec  t where
    TestPassSpec t = Pass.BasicPassSpec t

-- Pass.cache_phase1 ''TestPass
-- Pass.cache_phase2 ''TestPass


-- === API === --

-- type OnDemandPass pass = (Typeable pass, Pass.Compile pass IO)

-- runPass :: ∀ pass. OnDemandPass pass => Pass pass IO () -> IO ()
-- runPass = runPasses . pure

-- runPasses :: ∀ pass. OnDemandPass pass => [Pass pass IO ()] -> IO ()
-- runPasses passes = Scheduler.runManual registry $ do
--     Scheduler.debugRunPassDefs passes
--     where registry = do
--               Runner.registerAll


-- run2Passes :: ∀ pass. OnDemandPass pass => Pass pass IO () -> Pass pass IO () -> IO ()
-- run2Passes p1 p2 = runPasses [p1,p2]

-- runPass' :: Pass TestPass IO () -> IO ()
-- runPass' = runPass

-- run2Passes' :: Pass TestPass IO () -> Pass TestPass IO () -> IO ()
-- run2Passes' p1 p2 = runPasses [p1,p2]



--------------------------------
-- === Running benchmarks === --
--------------------------------

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    --     VectorSet.test
--     initializeTime

--     Containers.main
--     Data.main

--     Test.main
    IRBench.main

    -- defaultMain
    -- --     [ bgroup "CppContainers"
    -- --         [ bgroup "Data.IntSet"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ nfIO (SetTest.testInsertAndLookupIntSet $ 10^i)) <$> expSizes
    -- --         , bgroup "Data.PtrSet.Cpp"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ nfIO (SetTest.testInsertAndLookupCSet $ 10^i)) <$> expSizes
    -- --         , bgroup "Test.Data.PtrSet.Cpp"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ nfIO (SetTest.testInsertAndLookupForeignSet $ 10^i)) <$> expSizes
    -- --         , bgroup "testWithArrayLen"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ nfIO (SetTest.testWithArrayLen $ 10^i)) <$> expSizes
    -- --         , bgroup "FFICost"
    -- --             [ bgroup "Data.PtrSet.Cpp"
    -- --                 $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --                 $ nfIO (SetTest.testInsertLookupOrderedCSet $ 10^i)) <$> expSizes
    -- --             , bgroup "Test.Data.PtrSet.Cpp"
    -- --                 $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --                 $ nfIO (SetTest.testInsertLookupOrderedForeignSet $ 10^i)) <$> expSizes
    -- --             ]
    -- --         ]
    --     [ bgroup "IR"
    --         -- [ bgroup "Create node"
    --         --     $ (\(i :: Int) -> bench ("10e" <> show i)
    --         --     $ perRunEnv (return ())
    --         --     $ \v -> Basic.test_createNode (10 ^ i))  <$> [minExpVec..maxExpVec]

    --         -- , bgroup "Malloc ptr"
    --         --     $ (\(i :: Int) -> bench ("10e" <> show i)
    --         --     $ perRunEnv (return ())
    --         --     $ \v -> Basic.test_mallocPtr (10 ^ i))  <$> [minExpVec..maxExpVec]

    --         [ bgroup "ghc 8.4 bug"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Basic.ghc84bug (10 ^ i))  <$> [minExpVec..maxExpVec]

    --         , bgroup "ghc 8.4 bug 2"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Basic.ghc84bug2 (10 ^ i))  <$> [minExpVec..maxExpVec]

    --         , bgroup "Read/Write Layer + State config 4"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Basic.test_readWriteLayer4 (10 ^ i))  <$> [minExpVec..maxExpVec]

    --         -- , bgroup "Read/Write Layer + State config 3"
    --         --     $ (\(i :: Int) -> bench ("10e" <> show i)
    --         --     $ perRunEnv (return ())
    --         --     $ \v -> Basic.test_readWriteLayer3 (10 ^ i))  <$> [minExpVec..maxExpVec]

    --         , bgroup "Read/Write Layer + State config 2"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Basic.test_readWriteLayer2 (10 ^ i))  <$> [minExpVec..maxExpVec]

    --         , bgroup "Read/Write Layer + State"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Basic.test_readWriteLayer_ptrBuffOff (10 ^ i))  <$> [minExpVec..maxExpVec]

    -- --
    -- --         [ bgroup "Vector creation Hardcoded"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.test_VectorCreationHardcoded (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "List creation Hardcoded"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.test_ListCreationHardcoded (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Vector creation"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.test_VectorCreation (10 ^ i))  <$> [5]
    -- --
    -- --         , bgroup "List creation"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.test_ListCreation (10 ^ i))  <$> [5]
    -- --
    -- --         , bgroup "Pure loop"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.pureLoop (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "TS3 X"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> TS3.pureLoop_X (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "TS3 Z"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> TS3.pureLoop_Z (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Tup X"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> TT.pureLoop_X2 (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Tup Z"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> TT.pureLoop_Z2 (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "GADTs X"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> TT.pureLoop_X (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "GADTs Z"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> TT.pureLoop_Z (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Read/Write Layer static"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Basic.test_readWriteLayer_static (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Read/Write Layer + State config from Ptr"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Basic.test_readWriteLayer_ptrOff (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Read/Write Layer + State config from BuffPtr"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Basic.test_readWriteLayer_ptrBuffOff (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    --         , bgroup "Read/Write Ptr"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.readWritePtr (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Read/Write T Ptr"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.readWritePtr_T (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Read/Write T Ptr"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.readWritePtr_T2 (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Read/Write IORef"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.readWriteIORef (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Read/Write T IORef"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.readWriteIORef_T (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Read/Write Layer"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Basic.test_readWriteLayer (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    --         ]
    --         ]
    -- --     , bgroup "Storable"
    -- --         [ bgroup "Single storable"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.test_singleStorable (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --         , bgroup "Partial storable"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.test_partialStorable (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --         ]
    -- --     , bgroup "Construction"
    -- --         -- [ bgroup "IORefU"
    -- --         --     $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --         --     $ perRunEnv (return ())
    -- --         --     $ \v -> Test.test_newIORefU (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --         --
    -- --         -- , bgroup "IORef"
    -- --         --     $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --         --     $ perRunEnv (return ())
    -- --         --     $ \v -> Test.test_newIORef (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --         [ bgroup "ForeignPtr"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.test_mallocForeignPtr (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         --
    -- --         -- ,  bgroup "StablePtr"
    -- --         --     $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --         --     $ perRunEnv (return ())
    -- --         --     $ \v -> Test.test_mallocSPtr (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --         --
    -- --         ,  bgroup "malloc 1 (+ free)"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.test_malloc1 (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         ,  bgroup "malloc 100 (+ free)"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.test_malloc100 (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --         ]
    -- --
    -- --     , bgroup "Read+Write"
    -- --         [ bgroup "Pure loop"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.pureLoop (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "IORef"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.readWriteIORef (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "IORefU"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.readWriteIORefU (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "ForeignPtr"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.readWriteForeignPtr (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Vector"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.readWriteVector (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Ptr"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.readWritePtr (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --         ]
    -- --
    -- --     , bgroup "Fill"
    -- --         [ bgroup "MAutoVector with Int"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \x -> do{v <- unsafeNew (10^i + 1); Test.fillMAutoVector_Int (10 ^ i) v})  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "MVector with Int"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (Vector.unsafeNew (10 ^ i))
    -- --             $ (Test.fillMVector_Int (10 ^ i)))  <$> [minExpVec..maxExpVec]
    -- --         ]
    -- --     ]

