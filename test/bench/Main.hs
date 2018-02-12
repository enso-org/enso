{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-orphans -fno-warn-unused-binds #-}

module Main where

import Prologue

import qualified Data.Vector.Storable         as Vector
import qualified Data.Vector.Storable.Mutable as Vector
import qualified Test.Vector                  as Test

import Criterion.Main
import Criterion.Measurement            (initializeTime, getTime)
import Data.AutoVector.Storable.Mutable as AVector
import Data.Vector.Storable             (Vector)
import Data.Vector.Storable.Mutable     (MVector, IOVector, STVector)
import Luna.IR.Term
import System.IO                        (hSetBuffering, stdout, BufferMode(NoBuffering))
import Unsafe.Coerce                    (unsafeCoerce)

import qualified Data.Graph as Graph

import qualified Foreign.Memory.Pool as MemPool

import qualified Luna.IR.Term.Basic as Basic

timeIt :: MonadIO m => String -> m a -> m a
timeIt name f = do
    t1  <- liftIO getTime
    out <- f
    t2  <- liftIO getTime
    liftIO $ putStrLn (name <> ": " <> show (t2-t1))
    pure out


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    initializeTime


    -- (v :: AVector.MAutoVector' IO Int) <- unsafeNew 2
    -- print v
    -- -- AVector.
    --
    -- let consTestSize = 10^(8::Int)
    -- timeIt "r1" $ Test.fillMAutoVector_Int consTestSize =<< unsafeNew (consTestSize + 1)
    -- timeIt "r2" $ Test.fillMVector_Int     consTestSize =<< Vector.unsafeNew consTestSize

    -- MemPool.test
    -- MemPool.test2
    let minExpVec = 7
        maxExpVec = 8
    return ()

    Basic.test
    --
    defaultMain
        [ bgroup "IR"
            [ bgroup "Read/Write Ptr"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (return ())
                $ \v -> Test.readWritePtr (10 ^ i))  <$> [minExpVec..maxExpVec]

            , bgroup "Read/Write Layer"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (return ())
                $ \v -> Basic.test_readWriteLayer (10 ^ i))  <$> [minExpVec..maxExpVec]
            ]

        , bgroup "Storable"
            [ bgroup "Single storable"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (return ())
                $ \v -> Test.test_singleStorable (10 ^ i))  <$> [minExpVec..maxExpVec]
            , bgroup "Partial storable"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (return ())
                $ \v -> Test.test_partialStorable (10 ^ i))  <$> [minExpVec..maxExpVec]
            ]
        , bgroup "Construction"
            -- [ bgroup "IORefU"
            --     $ (\(i :: Int) -> bench ("10e" <> show i)
            --     $ perRunEnv (return ())
            --     $ \v -> Test.test_newIORefU (10 ^ i))  <$> [minExpVec..maxExpVec]
            --
            -- , bgroup "IORef"
            --     $ (\(i :: Int) -> bench ("10e" <> show i)
            --     $ perRunEnv (return ())
            --     $ \v -> Test.test_newIORef (10 ^ i))  <$> [minExpVec..maxExpVec]
            [ bgroup "ForeignPtr"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (return ())
                $ \v -> Test.test_mallocForeignPtr (10 ^ i))  <$> [minExpVec..maxExpVec]

            --
            -- ,  bgroup "StablePtr"
            --     $ (\(i :: Int) -> bench ("10e" <> show i)
            --     $ perRunEnv (return ())
            --     $ \v -> Test.test_mallocSPtr (10 ^ i))  <$> [minExpVec..maxExpVec]
            --
            ,  bgroup "malloc 1 (+ free)"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (return ())
                $ \v -> Test.test_malloc1 (10 ^ i))  <$> [minExpVec..maxExpVec]

            ,  bgroup "malloc 100 (+ free)"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (return ())
                $ \v -> Test.test_malloc100 (10 ^ i))  <$> [minExpVec..maxExpVec]
            ]

        , bgroup "Read+Write"
            [ bgroup "Pure loop"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (return ())
                $ \v -> Test.pureLoop (10 ^ i))  <$> [minExpVec..maxExpVec]

            , bgroup "IORef"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (return ())
                $ \v -> Test.readWriteIORef (10 ^ i))  <$> [minExpVec..maxExpVec]

            , bgroup "IORefU"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (return ())
                $ \v -> Test.readWriteIORefU (10 ^ i))  <$> [minExpVec..maxExpVec]

            , bgroup "ForeignPtr"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (return ())
                $ \v -> Test.readWriteForeignPtr (10 ^ i))  <$> [minExpVec..maxExpVec]

            , bgroup "Vector"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (return ())
                $ \v -> Test.readWriteVector (10 ^ i))  <$> [minExpVec..maxExpVec]

            , bgroup "Ptr"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (return ())
                $ \v -> Test.readWritePtr (10 ^ i))  <$> [minExpVec..maxExpVec]
            ]

        , bgroup "Fill"
            [ bgroup "Graph with Int"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (return ())
                $ \v -> Test.fillGraph (10 ^ i))  <$> [minExpVec..maxExpVec]

            , bgroup "MAutoVector with Int"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (return ())
                $ \x -> do{v <- unsafeNew (10^i + 1); Test.fillMAutoVector_Int (10 ^ i) v})  <$> [minExpVec..maxExpVec]

            , bgroup "MVector with Int"
                $ (\(i :: Int) -> bench ("10e" <> show i)
                $ perRunEnv (Vector.unsafeNew (10 ^ i))
                $ (Test.fillMVector_Int (10 ^ i)))  <$> [minExpVec..maxExpVec]
            ]
        ]
