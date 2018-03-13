{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-orphans -fno-warn-unused-binds #-}

module Test.Bench where

import Prologue

import qualified Data.Vector.Storable         as Vector
import qualified Data.Vector.Storable.Mutable as Vector
import qualified Test.Bench.Vector            as Test

import Criterion.Main
import Criterion.Measurement            (getTime, initializeTime)
import Data.AutoVector.Storable.Mutable as AVector
import Data.Vector.Storable             (Vector)
import Data.Vector.Storable.Mutable     (IOVector, MVector, STVector)
import System.IO                        (BufferMode (NoBuffering),
                                         hSetBuffering, stdout)
import Test.Pass
import Unsafe.Coerce                    (unsafeCoerce)

-- import qualified Data.GraphLuna as Graph

import qualified Foreign.Memory.Pool as MemPool

import qualified ToRefactor as Basic

import qualified Data.TypeSet3               as TS3
import           Language.Haskell.TH.Builder ()
import           Type.Data.Map

import qualified Test.Bench.MemoryManager as MManager
import qualified Test.Data.IntSet.Cpp     as SetTest

import Luna.Pass ()



timeIt :: MonadIO m => String -> m a -> m a
timeIt name f = do
    t1  <- liftIO getTime
    out <- f
    t2  <- liftIO getTime
    liftIO $ putStrLn (name <> ": " <> show (t2-t1))
    pure out

{-# ANN main ("HLint: ignore Use ." :: String) #-}
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    initializeTime

    -- MManager.runBenchmarks
    Basic.test
    TS3.test
    print "---"
    Basic.passTest_run
    -- Basic.passRunTest
    -- Basic.test_pm_run

    let minExpVec = 7 :: Int
        maxExpVec = 7 :: Int
        expSizes  = [minExpVec .. maxExpVec] :: [Int]

    defaultMain
    --     [ bgroup "CppContainers"
    --         [ bgroup "Data.IntSet"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ nfIO (SetTest.testInsertAndLookupIntSet $ 10^i)) <$> expSizes
    --         , bgroup "Data.IntSet.Cpp"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ nfIO (SetTest.testInsertAndLookupCSet $ 10^i)) <$> expSizes
    --         , bgroup "Test.Data.IntSet.Cpp"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ nfIO (SetTest.testInsertAndLookupForeignSet $ 10^i)) <$> expSizes
    --         , bgroup "testWithArrayLen"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ nfIO (SetTest.testWithArrayLen $ 10^i)) <$> expSizes
    --         , bgroup "FFICost"
    --             [ bgroup "Data.IntSet.Cpp"
    --                 $ (\(i :: Int) -> bench ("10e" <> show i)
    --                 $ nfIO (SetTest.testInsertLookupOrderedCSet $ 10^i)) <$> expSizes
    --             , bgroup "Test.Data.IntSet.Cpp"
    --                 $ (\(i :: Int) -> bench ("10e" <> show i)
    --                 $ nfIO (SetTest.testInsertLookupOrderedForeignSet $ 10^i)) <$> expSizes
    --             ]
    --         ]
        [ bgroup "IR"
            -- [ bgroup "Create node"
            --     $ (\(i :: Int) -> bench ("10e" <> show i)
            --     $ perRunEnv (return ())
            --     $ \v -> Basic.test_createNode (10 ^ i))  <$> [minExpVec..maxExpVec]

            -- , bgroup "Malloc ptr"
            --     $ (\(i :: Int) -> bench ("10e" <> show i)
            --     $ perRunEnv (return ())
            --     $ \v -> Basic.test_mallocPtr (10 ^ i))  <$> [minExpVec..maxExpVec]

            [ bgroup "Read/Write Layer + State config 4"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (return ())
                $ \v -> Basic.test_readWriteLayer4 (10 ^ i))  <$> [minExpVec..maxExpVec]

            -- , bgroup "Read/Write Layer + State config 3"
            --     $ (\(i :: Int) -> bench ("10e" <> show i)
            --     $ perRunEnv (return ())
            --     $ \v -> Basic.test_readWriteLayer3 (10 ^ i))  <$> [minExpVec..maxExpVec]

            , bgroup "Read/Write Layer + State config 2"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (return ())
                $ \v -> Basic.test_readWriteLayer2 (10 ^ i))  <$> [minExpVec..maxExpVec]

            , bgroup "Read/Write Layer + State"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (return ())
                $ \v -> Basic.test_readWriteLayer_ptrBuffOff (10 ^ i))  <$> [minExpVec..maxExpVec]

    --
    --         [ bgroup "Vector creation Hardcoded"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.test_VectorCreationHardcoded (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "List creation Hardcoded"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.test_ListCreationHardcoded (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "Vector creation"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.test_VectorCreation (10 ^ i))  <$> [5]
    --
    --         , bgroup "List creation"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.test_ListCreation (10 ^ i))  <$> [5]
    --
    --         , bgroup "Pure loop"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.pureLoop (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "TS3 X"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> TS3.pureLoop_X (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "TS3 Z"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> TS3.pureLoop_Z (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "Tup X"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> TT.pureLoop_X2 (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "Tup Z"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> TT.pureLoop_Z2 (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "GADTs X"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> TT.pureLoop_X (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "GADTs Z"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> TT.pureLoop_Z (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "Read/Write Layer static"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Basic.test_readWriteLayer_static (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "Read/Write Layer + State config from Ptr"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Basic.test_readWriteLayer_ptrOff (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "Read/Write Layer + State config from BuffPtr"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Basic.test_readWriteLayer_ptrBuffOff (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
            , bgroup "Read/Write Ptr"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (return ())
                $ \v -> Test.readWritePtr (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "Read/Write T Ptr"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.readWritePtr_T (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "Read/Write T Ptr"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.readWritePtr_T2 (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "Read/Write IORef"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.readWriteIORef (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "Read/Write T IORef"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.readWriteIORef_T (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "Read/Write Layer"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Basic.test_readWriteLayer (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
            ]
            ]
    --     , bgroup "Storable"
    --         [ bgroup "Single storable"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.test_singleStorable (10 ^ i))  <$> [minExpVec..maxExpVec]
    --         , bgroup "Partial storable"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.test_partialStorable (10 ^ i))  <$> [minExpVec..maxExpVec]
    --         ]
    --     , bgroup "Construction"
    --         -- [ bgroup "IORefU"
    --         --     $ (\(i :: Int) -> bench ("10e" <> show i)
    --         --     $ perRunEnv (return ())
    --         --     $ \v -> Test.test_newIORefU (10 ^ i))  <$> [minExpVec..maxExpVec]
    --         --
    --         -- , bgroup "IORef"
    --         --     $ (\(i :: Int) -> bench ("10e" <> show i)
    --         --     $ perRunEnv (return ())
    --         --     $ \v -> Test.test_newIORef (10 ^ i))  <$> [minExpVec..maxExpVec]
    --         [ bgroup "ForeignPtr"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.test_mallocForeignPtr (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         --
    --         -- ,  bgroup "StablePtr"
    --         --     $ (\(i :: Int) -> bench ("10e" <> show i)
    --         --     $ perRunEnv (return ())
    --         --     $ \v -> Test.test_mallocSPtr (10 ^ i))  <$> [minExpVec..maxExpVec]
    --         --
    --         ,  bgroup "malloc 1 (+ free)"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.test_malloc1 (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         ,  bgroup "malloc 100 (+ free)"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.test_malloc100 (10 ^ i))  <$> [minExpVec..maxExpVec]
    --         ]
    --
    --     , bgroup "Read+Write"
    --         [ bgroup "Pure loop"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.pureLoop (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "IORef"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.readWriteIORef (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "IORefU"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.readWriteIORefU (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "ForeignPtr"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.readWriteForeignPtr (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "Vector"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.readWriteVector (10 ^ i))  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "Ptr"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.readWritePtr (10 ^ i))  <$> [minExpVec..maxExpVec]
    --         ]
    --
    --     , bgroup "Fill"
    --         [ bgroup "MAutoVector with Int"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \x -> do{v <- unsafeNew (10^i + 1); Test.fillMAutoVector_Int (10 ^ i) v})  <$> [minExpVec..maxExpVec]
    --
    --         , bgroup "MVector with Int"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (Vector.unsafeNew (10 ^ i))
    --             $ (Test.fillMVector_Int (10 ^ i)))  <$> [minExpVec..maxExpVec]
    --         ]
    --     ]
