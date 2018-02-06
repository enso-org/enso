{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-orphans -fno-warn-unused-binds #-}
-- {-# LANGUAGE Strict #-}

module Main where

import           Prelude as P hiding (mempty, length, (.))

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
import           Luna.Core.Store
import qualified Luna.Core.StorableVector     as SVec
import qualified Luna.Core.UnboxedVector      as UVec


main :: IO ()
main = do
    vx <- alloc (10^(8::Int) + 1) -- FIXME: it should be done in env

    defaultMain [
          bgroup "StorableVector.mknodes"
              $ (\(i :: Int) -> env (SVec.mkVec (10 ^ i))
              $ \v -> bench ("10e" <> show i)
              $ nfIO (SVec.mknodes  (10 ^ i) v))  <$> [7..8]
        , bgroup "StorableVector.mknodesU (unsafe)"
              $ (\(i :: Int) -> env (SVec.mkVec (10 ^ i))
              $ \v -> bench ("10e" <> show i)
              $ nfIO (SVec.mknodes  (10 ^ i) v))  <$> [7..8]
        , bgroup "StorableVector.mknodes2 (pre-allocated)"
              $ (\(i :: Int) -> env (return ())
              $ \v -> bench ("10e" <> show i)
              $ nfIO (SVec.mknodes2 (10 ^ i) vx)) <$> [7..8]
        , bgroup "StorableArray.mknodes"
              $ (\(i :: Int) -> env (SArr.mkArray (10 ^ i))
              $ \v -> bench ("10e" <> show i)
              $ nfIO (SArr.mknodes  (10 ^ i) v))  <$> [7..8]
        , bgroup "UnboxedVector.mknodes"
              $ (\(i :: Int) -> env (UVec.mkVec (10 ^ i))
              $ \v -> bench ("10e" <> show i)
              $ nfIO (UVec.mknodes  (10 ^ i) v))  <$> [7..8]
        ]

    let v :: Vector (UniCore ())
        v = runST $ do
            nodes <- Vector.new 10
            Vector.write nodes 0 (UAcc $ Acc (ULink (Keyx 1)) (ULink (Keyx 2)))
            Vector.write nodes 1 (UAcc $ Acc (ULink (Keyx 17)) (ULink (Keyx 27)))
            Vector.write nodes 2 (UAcc $ Acc (ULink (Keyx 15)) (ULink (Keyx 25)))
            Vector.freeze nodes
    print $ Vector.unsafeIndex v 0
    print $ Vector.unsafeIndex (unsafeCoerce v :: Vector (Spec (Acc ()))) 0
    print $ Vector.unsafeIndex (unsafeCoerce v :: Vector (Spec (Acc ()))) 1
    print $ Vector.unsafeIndex (unsafeCoerce v :: Vector (Spec (Acc ()))) 2
    print $ alignment' @Int
    print $ alignment' @Char
    print $ alignment' @Bool
