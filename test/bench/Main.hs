{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-orphans -fno-warn-unused-binds #-}
-- {-# LANGUAGE Strict #-}

module Main where

import           Prelude as P hiding (mempty, length)

import           Criterion.Main
import           Control.DeepSeq
import           Control.Monad.ST             (runST)
import           Data.Monoids

import qualified Data.Vector.Storable         as Vector hiding (length)
import           Data.Vector.Storable         (Vector)
import qualified Data.Vector.Storable.Mutable as Vector
import           Data.Vector.Storable.Mutable (MVector, IOVector, STVector)
import           Unsafe.Coerce                (unsafeCoerce)

import qualified Luna.Core.Array              as SArr
import           Luna.Core.Data
import           Luna.Core.ST
import Data.AutoVector.Storable.Mutable
import qualified Test.Vector     as Test
-- import qualified Luna.Core.UnboxedVector      as UVec


import qualified Data.IntList.Storable.Mutable as ListM



import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.Int (Int64)


import Criterion.Measurement (initializeTime, getTime)

import Control.Monad.IO.Class

timeIt :: MonadIO m => String -> m a -> m a
timeIt name f = do
    t1  <- liftIO getTime
    out <- f
    t2  <- liftIO getTime
    liftIO $ putStrLn (name <> ": " <> show (t2-t1))
    return out


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    initializeTime

    timeIt "r1" $ Test.fillMAutoVector (10^8) =<< alloc (10^8 + 1)
    timeIt "r2" $ Test.fillMVector     (10^8) =<< Vector.unsafeNew (10 ^ 8)

    let minExpVec = 7
        maxExpVec = 8

    defaultMain
        [ bgroup "MAutoVector"
            $ (\(i :: Int) -> bench ("10e" <> show i)
            $ perRunEnv (alloc (10^i + 1))
            $ \v -> Test.fillMAutoVector (10 ^ i) v)  <$> [minExpVec..maxExpVec]

        , bgroup "MVector"
            $ (\(i :: Int) -> bench ("10e" <> show i)
            $ perRunEnv (Vector.unsafeNew (10 ^ i))
            $ (Test.fillMVector (10 ^ i)))  <$> [minExpVec..maxExpVec]
        ]
