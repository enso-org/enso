{-# LANGUAGE UnliftedFFITypes #-}
module Luna.Test.Bench.MemoryManager where

import Prelude

import Control.DeepSeq       (NFData)
import Criterion.Main
import Foreign               (Ptr)
import Foreign.C             (CDouble (..), CSize (..))
import Foreign.Marshal.Alloc (free, mallocBytes)

import Foreign.Memory.Manager

--------------------------------------
-- === Foreign imports from C++ === --
--------------------------------------

foreign import ccall unsafe "benchmark"           c_benchmark           :: CSize -> CSize -> IO()
foreign import ccall unsafe "randomizedBenchmark" c_randomizedBenchmark :: CSize -> CSize -> CDouble -> IO()
foreign import ccall unsafe "justReturn"          c_justReturn          :: CSize -> IO (Ptr ())



------------------------
-- === Benchmarks === --
------------------------

-- Benchmarks IO action that uses MemoryManager
benchmarkWithManager :: NFData a
                     => String -> (MemoryManager -> IO a) -> Int -> Int
                     -> Benchmark
benchmarkWithManager name action blockSize itemSize = envWithCleanup
    (newManager blockSize itemSize)
    deleteManager
    (bench name . nfIO . action)

testAllocFreePairsCpp :: Int -> Int -> Benchmark
testAllocFreePairsCpp = benchmarkWithManager
    "FFI: C++ manager (new >>= delete)"
    (\mgr -> newItem mgr >>= deleteItem mgr)

runBenchmarks :: IO ()
runBenchmarks = defaultMain
    [ testAllocFreePairsCpp 1024 64
    , benchmarkWithManager "FFI: C++ manager (new)" newItem 1024 64
    , bench "FFI: trivial call" $ nfIO (c_justReturn 5) -- TODO allocates much memory, so for higher iteration count there will be slow downs
    , bench "Foreign.Marshal.Alloc" $ nfIO (mallocBytes 64 >>= free)
    , bench "C++: randomized pattern" $ nfIO (c_randomizedBenchmark 10000000 64 0.7)
    ]
    -- benchmark 10000000 64

