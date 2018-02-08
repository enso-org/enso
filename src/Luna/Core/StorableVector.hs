{-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}
{-# LANGUAGE Strict #-}
module Luna.Core.StorableVector where

import Prelude

import qualified Data.Vector.Storable as Vector hiding (length)
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable.Mutable as Vector
import           Data.Vector.Storable.Mutable (MVector, IOVector, STVector)

import Luna.Core.Data
import Luna.Core.Store2
import Control.Monad (when)
import Control.Lens.Utils

import Control.Monad.Primitive (PrimState)

mknodes_thawFreeze :: Int -> MVector (PrimState IO) (UniCore ()) -> IO ()
mknodes_thawFreeze !i !v = do
    let go j = do
          x <- if j == 0 then return 0 else do
            pd <- Vector.unsafeRead v (j - 1)
            return $ fromSampleData pd
          Vector.unsafeWrite v j (mkSampleData (x+1) (x+1))
          when (j < i - 1) $ go (j + 1)
    go 0
    -- print =<< Vector.unsafeRead v (i - 1)

mknodes_thawFreeze2 :: Int -> StoreM' IO (UniCore ()) -> IO ()
mknodes_thawFreeze2 !i !s = do
    let go j = do
          x <- if j == 0 then return 0 else do
            pd <- Vector.unsafeRead (s ^. vector) (j - 1)
            return $ fromSampleData pd
          Vector.unsafeWrite (s ^. vector) j (mkSampleData (x+1) (x+1))
          when (j < i - 1) $ go (j + 1)
    go 0
    -- print =<< Vector.unsafeRead (s ^. vector) (i - 1)
    return()

-- mknodes_thawFreeze2 :: Int -> StoreM' IO (UniCore ()) -> IO ()
-- mknodes_thawFreeze2 !i !s = do
--     let go j v = do
--           (v', (k :: Int)) <- reserveKey v
--           x <- if j == 0 then return 0 else do
--             pd <- Vector.unsafeRead (v' ^. vector) (j - 1)
--             return $ fromSampleData pd
--           Vector.unsafeWrite (v' ^. vector) k (mkSampleData (x+1) (x+1))
--           when (j < i - 1) $ go (j + 1) v'
--     go 0 s
--     return()


    -- print =<< Vector.unsafeRead nodes (i - 1)
    -- Vector.unsafeFreeze nodes

-- mknodes2 :: Int -> StoreM' IO (UniCore ()) -> IO ()
-- mknodes2 !i !s = do
--     let go 0 _  = return ()
--         go j !v = do
--             (v', (k :: Int)) <- reserveKey v
--             unsafeWriteSpec v' k (mkSampleData j (j+1))
--             go (j - 1) v'
--     go i s
--     return ()


mkVec :: Int -> IO (Vector (UniCore ()))
mkVec !i = Vector.unsafeFreeze =<< Vector.new (i + 1)


mkVecU :: Int -> IO (IOVector (UniCore ()))
mkVecU !i = Vector.unsafeNew (i + 1)
