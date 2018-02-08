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
import           Luna.Core.Store2
import qualified Luna.Core.StorableVector     as SVec
-- import qualified Luna.Core.UnboxedVector      as UVec


import qualified Data.IntList.Storable.Mutable as ListM



import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.Int (Int64)


import Criterion.Measurement (initializeTime, getTime)


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    initializeTime
    v <- alloc (10^8 + 1)
    t1 <- getTime
    SVec.mknodes_thawFreeze2 (10^8) v
    t2 <- getTime
    print ("r1 " ++ show (t2-t1))

    v <- Vector.unsafeNew (10 ^ 8)
    t1 <- getTime
    SVec.mknodes_thawFreeze (10 ^ 8) v
    t2 <- getTime
    print ("r2 " ++ show (t2-t1))

    lst <- ListM.new
    ListM.unshift lst 5
    ListM.unshift lst 6
    print lst
    print =<< ListM.shift lst
    print =<< ListM.shift lst
    print =<< ListM.shift lst
    print =<< ListM.shift lst
    print =<< ListM.shift lst

    -- let a = LL.
    -- vx <- alloc (10^(8::Int) + 1) -- FIXME: it should be done in env
    -- vx2 <- alloc (10^(8::Int) + 1) -- FIXME: it should be done in env

    defaultMain
        [ bgroup "Storable.MVector2"
            $ (\(i :: Int) -> bench ("10e" <> show i)
            $ perRunEnv (alloc (10^i + 1))
            $ \v -> SVec.mknodes_thawFreeze2 (10 ^ i) v)  <$> [7..8]

        , bgroup "Storable.MVector"
            $ (\(i :: Int) -> bench ("10e" <> show i)
            $ perRunEnv (Vector.unsafeNew (10 ^ i))
            $ (SVec.mknodes_thawFreeze (10 ^ i)))  <$> [7..8]
        --
        -- , bgroup "StorableVector.mknodes2"
        --       $ (\(i :: Int) -> env (return ())
        --       $ \v -> bench ("10e" <> show i)
        --       $ nfIO (SVec.mknodes2 (10 ^ i) vx)) <$> [7..8]
        -- , bgroup "StorableArray.mknodes"
        --       $ (\(i :: Int) -> env (SArr.mkArray (10 ^ i))
        --       $ \v -> bench ("10e" <> show i)
        --       $ nfIO (SArr.mknodes  (10 ^ i) v))  <$> [7..8]
        -- , bgroup "UnboxedVector.mknodes"
        --       $ (\(i :: Int) -> env (UVec.mkVec (10 ^ i))
        --       $ \v -> bench ("10e" <> show i)
        --       $ nfIO (UVec.mknodes  (10 ^ i) v))  <$> [7..8]
        ]

    let v :: Vector (UniCore ())
        v = runST $ do
            nodes <- Vector.new 10
            Vector.write nodes 0 (mkSampleData 1  2)
            Vector.write nodes 1 (mkSampleData 17 27)
            Vector.write nodes 2 (mkSampleData 15 25)
            Vector.freeze nodes
    print $ Vector.unsafeIndex v 0
    -- print $ Vector.unsafeIndex (unsafeCoerce v :: Vector (Spec (Acc ()))) 0
    -- print $ Vector.unsafeIndex (unsafeCoerce v :: Vector (Spec (Acc ()))) 1
    -- print $ Vector.unsafeIndex (unsafeCoerce v :: Vector (Spec (Acc ()))) 2
