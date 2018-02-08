{-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}
{-# LANGUAGE Strict #-}
module Test.Vector where

import Prelude

import qualified Data.Vector.Storable as Vector hiding (length)
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable.Mutable as Vector
import           Data.Vector.Storable.Mutable (MVector, IOVector, STVector)

import Luna.Core.Data
import Data.AutoVector.Storable.Mutable
import Control.Monad (when)
import Control.Lens.Utils
import Control.Monad.Primitive (PrimState)


fillMVector :: Int -> MVector (PrimState IO) (UniCore ()) -> IO ()
fillMVector !i !v = do
    let go j = do
          x <- if j == 0 then return 0 else do
            pd <- Vector.unsafeRead v (j - 1)
            return $ fromSampleData pd
          Vector.unsafeWrite v j (mkSampleData (x+1) (x+1))
          when (j < i - 1) $ go (j + 1)
    go 0
    -- print =<< Vector.unsafeRead v (i - 1)
{-# NOINLINE fillMVector #-}


fillMAutoVector :: Int -> MAutoVector' IO (UniCore ()) -> IO ()
fillMAutoVector !i !s = do
    let go j = do
          k <- reserveKey s
          x <- if j == 0 then return 0 else do
            pd <- Vector.unsafeRead (s ^. vector) (j - 1)
            return $ fromSampleData pd
          Vector.unsafeWrite (s ^. vector) k (mkSampleData (x+1) (x+1))
          when (j < i - 1) $ go (j + 1)
    go 0
    -- print =<< Vector.unsafeRead (s ^. vector) (i - 1)
    return()
{-# NOINLINE fillMAutoVector #-}
