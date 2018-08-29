{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Benchmark.Statistics.Internal where

import Prologue

import Data.List as List



-----------------
-- === API === --
-----------------

meanI :: Integral a => [a] -> a
meanI !xs
    = if length xs < 1 then 0 else foldl' (+) 0 xs `div` List.genericLength xs
{-# INLINE meanI #-}

meanF :: Floating a => [a] -> a
meanF !xs
    = if length xs < 1 then 0 else foldl' (+) 0 xs / List.genericLength xs
{-# INLINE meanF #-}

stddevI :: (Integral a, Floating b) => [a] -> b
stddevI !xs = let ys = fromIntegral <$> xs in stddevF ys
{-# INLINE stddevI #-}

stddevF :: Floating a => [a] -> a
stddevF !xs = if length xs < 2 then 0 else sqrt $ sumOfSquares / (n - 1) where
    n            = List.genericLength xs
    xBar         = meanF xs
    sumOfSquares = List.foldl' (+) 0 $ (\x -> (x - xBar) ** 2) <$> xs
{-# INLINE stddevF #-}

