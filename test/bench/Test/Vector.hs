{-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}
{-# LANGUAGE Strict #-}
module Test.Vector where

import Prologue

import qualified Data.Vector.Storable.Mutable as Vector

import Data.Vector.Storable.Mutable (MVector)
import OCI.IR.Term
import Data.AutoVector.Storable.Mutable

import qualified Data.Graph as Graph
import Data.IORef
import Foreign.ForeignPtr.Utils
import Control.Exception.Base (evaluate)

import qualified Foreign.Marshal.Utils as Ptr
import qualified Foreign.Marshal.Alloc as Ptr
import Foreign.Storable (peek, poke)

import qualified Foreign.StablePtr as SPtr

import Data.IORef.Unboxed


fillMVector_Int :: Int -> MVector (PrimState IO) Int -> IO ()
fillMVector_Int !i !v = do
    let go j = do
          x <- if j == 0 then pure 0 else Vector.unsafeRead v (j - 1)
          Vector.unsafeWrite v j (x+1)
          when_ (j < i - 1) $ go (j + 1)
    go 0
    -- print =<< Vector.unsafeRead v (i - 1)
{-# NOINLINE fillMVector_Int #-}


fillMAutoVector_Int :: Int -> MAutoVector' IO Int -> IO ()
fillMAutoVector_Int !i !s = do
    let go j = do
          k <- reserveIndex s
          x <- if j == 0 then pure 0 else Vector.unsafeRead (s ^. dataVector) (j - 1)
          Vector.unsafeWrite (s ^. dataVector) k (x+1)
          when_ (j < i - 1) $ go (j + 1)
    go 0
    -- print =<< Vector.unsafeRead (s ^. dataVector) (i - 1)
    pure()
{-# NOINLINE fillMAutoVector_Int #-}


fillGraph :: Int -> IO ()
fillGraph i = do
    let go :: Graph.Node Int -> Int -> IO ()
        go (~prev) j = do
          x <- if j == 0 then pure 0 else Graph.read prev
          n <- Graph.newNode (x+1)
          when_ (j < i - 1) $ go n (j + 1)
    go undefined 0
    -- print =<< Vector.unsafeRead (s ^. dataVector) (i - 1)
    pure()




pureLoop :: Int -> IO ()
pureLoop i = do
    ref <- newIORef (0 :: Int)
    let go x 0 = evaluate x
        go x j = do
            go (x+1) (j - 1)
    go (0 :: Int) i
    return ()

readWriteIORef :: Int -> IO ()
readWriteIORef i = do
    ref <- newIORef (0 :: Int)
    let go 0 = return ()
        go j = do
            x <- readIORef ref
            writeIORef ref (x+1)
            go (j - 1)
    go i


readWriteIORefU :: Int -> IO ()
readWriteIORefU i = do
    ref <- newIORefU (0 :: Int)
    let go 0 = return ()
        go j = do
            x <- readIORefU ref
            writeIORefU ref (x+1)
            go (j - 1)
    go i


readWriteForeignPtr :: Int -> IO ()
readWriteForeignPtr i = do
    ptr <- mkForeignPtr (0 :: Int)
    let go 0 = return ()
        go j = do
            x <- readForeignPtr ptr
            writeForeignPtr ptr (x+1)
            go (j - 1)
    go i

readWritePtr :: Int -> IO ()
readWritePtr i = do
    ptr <- Ptr.new (0 :: Int)
    let go 0 = return ()
        go j = do
            x <- peek ptr
            poke ptr (x+1)
            go (j - 1)
    go i
    Ptr.free ptr


readWriteVector :: Int -> IO ()
readWriteVector i = do
    vec <- Vector.unsafeNew 1
    Vector.unsafeWrite vec 0 (0 :: Int)
    let go 0 = return ()
        go j = do
            x <- Vector.unsafeRead vec 0
            Vector.unsafeWrite vec 0 (x+1)
            go (j - 1)
    go i


test_newIORef :: Int -> IO ()
test_newIORef i = do
    let go 0 = return ()
        go j = do
            ref <- newIORef (0 :: Int)
            go (j - 1)
    go i

test_newIORefU :: Int -> IO ()
test_newIORefU i = do
    let go 0 = return ()
        go j = do
            ref <- newIORefU (0 :: Int)
            go (j - 1)
    go i

test_mallocForeignPtr :: Int -> IO ()
test_mallocForeignPtr i = do
    let go 0 = return ()
        go j = do
            ref <- mkForeignPtr (0 :: Int)
            go (j - 1)
    go i

test_mallocPtr :: Int -> IO ()
test_mallocPtr i = do
    let go 0 = return ()
        go j = do
            ptr <- Ptr.new (0 :: Int)
            -- Ptr.free ptr
            go (j - 1)
    go i

test_mallocSPtr :: Int -> IO ()
test_mallocSPtr i = do
    let go 0 = return ()
        go j = do
            ptr <- SPtr.newStablePtr (0 :: Int)
            -- Ptr.free ptr
            go (j - 1)
    go i
