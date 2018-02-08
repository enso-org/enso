{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-orphans -fno-warn-unused-binds #-}
-- {-# LANGUAGE Strict #-}

module Main where

import Prologue
import           Criterion.Main

import qualified Data.Vector.Storable         as Vector
import qualified Data.Vector.Storable.Mutable as Vector

import Data.Vector.Storable         (Vector)
import Data.Vector.Storable.Mutable (MVector, IOVector, STVector)
import Unsafe.Coerce                (unsafeCoerce)

import           OCI.IR.Term
import Data.AutoVector.Storable.Mutable
import qualified Test.Vector     as Test
-- import qualified Luna.Core.UnboxedVector      as UVec


import qualified Data.IntList.Storable.Mutable as ListM



import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.Int (Int64)

import Criterion.Measurement (initializeTime, getTime)


foo :: IO Int
foo = do
    let x = 5
    print x
    print x
    print (x+1)
    return 7

timeIt :: MonadIO m => String -> m a -> m a
timeIt name f = do
    t1  <- liftIO getTime
    out <- f
    t2  <- liftIO getTime
    liftIO $ putStrLn (name <> ": " <> show (t2-t1))
    pure out


main :: IO ()
main = do
    y <- Test.voidx foo
    print y
    hSetBuffering stdout NoBuffering
    initializeTime

    let consTestSize = 10^(8::Int)
    timeIt "r1" $ Test.fillMAutoVector_Int consTestSize =<< alloc (consTestSize + 1)
    timeIt "r2" $ Test.fillMVector_Int     consTestSize =<< Vector.unsafeNew consTestSize

    let minExpVec = 6
        maxExpVec = 7

    defaultMain
        [ bgroup "Fill MAutoVector with Int"
            $ (\(i :: Int) -> bench ("10e" <> show i)
            $ perRunEnv (alloc (10^i + 1))
            $ \v -> Test.fillMAutoVector_Int (10 ^ i) v)  <$> [minExpVec..maxExpVec]

        , bgroup "Fill MVector with Int"
            $ (\(i :: Int) -> bench ("10e" <> show i)
            $ perRunEnv (Vector.unsafeNew (10 ^ i))
            $ (Test.fillMVector_Int (10 ^ i)))  <$> [minExpVec..maxExpVec]

        , bgroup "Fill MAutoVector with UniCore"
            $ (\(i :: Int) -> bench ("10e" <> show i)
            $ perRunEnv (alloc (10^i + 1))
            $ \v -> Test.fillMAutoVector_UniCore (10 ^ i) v)  <$> [minExpVec..maxExpVec]

        , bgroup "Fill MVector with UniCore"
            $ (\(i :: Int) -> bench ("10e" <> show i)
            $ perRunEnv (Vector.unsafeNew (10 ^ i))
            $ (Test.fillMVector_UniCore (10 ^ i)))  <$> [minExpVec..maxExpVec]
        ]
