{-# LANGUAGE UnliftedFFITypes #-}
module Test.MemoryManager where

import Prelude

import Criterion.Main
import Control.DeepSeq (NFData)
import Foreign (Ptr)
import Foreign.C (CDouble(..), CSize(..))
import Foreign.Marshal.Alloc (mallocBytes, free)

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
                     => String -> (MemoryManager -> IO a) -> Int
                     -> Benchmark
benchmarkWithManager name action itemSize = envWithCleanup
    (newManager itemSize)
    deleteManager
    (\mgr -> bench name $ nfIO (action mgr))

testAllocFreePairsCpp :: Int -> Benchmark
testAllocFreePairsCpp = benchmarkWithManager
    "FFI: C++ manager (new >>= delete)"
    (\mgr -> newItem mgr >>= deleteItem mgr)

runBenchmarks :: IO ()
runBenchmarks = do
    defaultMain
        [ testAllocFreePairsCpp 64
        , benchmarkWithManager "FFI: C++ manager (new)" newItem 64
        , bench "FFI: trivial call" $ nfIO $ c_justReturn 5 -- TODO allocates much memory, so for higher iteration count there will be slow downs
        , bench "Foreign.Marshal.Alloc" $ nfIO $ (mallocBytes 64 >>= free)
        , bench "C++: randomized pattern" $ nfIO $ c_randomizedBenchmark 10000000 64 0.7
        ]
    -- benchmark 10000000 64

