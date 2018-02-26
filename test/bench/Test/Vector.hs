-- {-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}
{-# LANGUAGE Strict #-}
module Test.Vector where

import Prologue

import qualified Data.Vector.Storable.Mutable as Vector

import Data.Vector.Storable.Mutable (MVector)
import qualified Data.Vector.Storable as V
import Luna.IR.Term
import Data.AutoVector.Storable.Mutable

-- import qualified Data.Graph as Graph
import Data.IORef
import Foreign.ForeignPtr.Utils
import Control.Exception.Base (evaluate)
import Foreign (Ptr, castPtr)

import qualified Foreign.Marshal.Utils as Ptr
import qualified Foreign.Marshal.Alloc as Ptr
import Foreign.Storable (Storable(..), peek, poke)
import Foreign.Storable.Utils

import qualified Foreign.StablePtr as SPtr

import Data.IORef.Unboxed

import qualified Foreign.Memory.Pool as MemPool


data Test = Test {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int{-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int{-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show)


ccc :: Int
ccc = sizeOf (undefined :: Int)

instance Storable Test where
    sizeOf ~_ = 15 * ccc ; {-# INLINE sizeOf #-}
    alignment ~_ = ccc ; {-# INLINE alignment #-}
    poke ptr (Test t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15) = poke (castPtr ptr) t1 >> pokeByteOff ptr ccc t2 >> pokeByteOff ptr (ccc*2) t3 >> pokeByteOff ptr (ccc*3) t4 >> pokeByteOff ptr (ccc*4) t5 >> pokeByteOff ptr (ccc*5) t6 >> pokeByteOff ptr (ccc*6) t7 >> pokeByteOff ptr (ccc*7) t8 >> pokeByteOff ptr (ccc*8) t9 >> pokeByteOff ptr (ccc*9) t10 >> pokeByteOff ptr (ccc*10) t11 >> pokeByteOff ptr (ccc*11) t12 >> pokeByteOff ptr (ccc*12) t13 >> pokeByteOff ptr (ccc*13) t14 >> pokeByteOff ptr (ccc*14) t15 ; {-# INLINE poke #-}
    peek ptr = Test <$> peek (castPtr ptr) <*> peekByteOff ptr ccc <*> peekByteOff ptr (ccc*2) <*> peekByteOff ptr (ccc*3) <*> peekByteOff ptr (ccc*4) <*> peekByteOff ptr (ccc*5) <*> peekByteOff ptr (ccc*6) <*> peekByteOff ptr (ccc*7) <*> peekByteOff ptr (ccc*8) <*> peekByteOff ptr (ccc*9) <*> peekByteOff ptr (ccc*10) <*> peekByteOff ptr (ccc*11) <*> peekByteOff ptr (ccc*12) <*> peekByteOff ptr (ccc*13) <*> peekByteOff ptr (ccc*14) ; {-# INLINE peek #-}



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


-- fillGraph :: Int -> IO ()
-- fillGraph i = do
--     let go :: Graph.Node Int -> Int -> IO ()
--         go (~prev) j = do
--           x <- if j == 0 then pure 0 else Graph.read prev
--           n <- Graph.newNode (x+1)
--           when_ (j < i - 1) $ go n (j + 1)
--     go undefined 0
--     -- print =<< Vector.unsafeRead (s ^. dataVector) (i - 1)
--     pure()




pureLoop :: Int -> IO ()
pureLoop i = do
    let go !x 0 = evaluate x
        go !x j = do
            let !y = x+1
            go y $! j - 1
    go (0 :: Int) i
    return ()

readWriteIORef :: Int -> IO ()
readWriteIORef i = do
    !ref <- newIORef (0 :: Int)
    let go 0 = return ()
        go !j = do
            !x <- readIORef ref
            writeIORef ref $! x+1
            go $! j - 1
    go i

readWriteIORef_T :: Int -> IO ()
readWriteIORef_T i = do
    !ref <- newIORef (Test 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
    let go 0 = return ()
        go !j = do
            !(Test t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15) <- readIORef ref
            writeIORef ref $! (Test (t1+1) t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15)
            go $! j - 1
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
            !x <- peek ptr
            poke ptr $! x+1
            go $! j - 1
    go i
    Ptr.free ptr

readWritePtr_T :: Int -> IO ()
readWritePtr_T i = do
    ptr <- Ptr.new (Test 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
    let go 0 = return ()
        go j = do
            !(Test t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15) <- peek ptr
            poke ptr $! (Test (t1+1) t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15)
            go $! j - 1
    go i
    Ptr.free ptr

readWritePtr_T2 :: Int -> IO ()
readWritePtr_T2 i = do
    ptr <- Ptr.new (Test 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
    let go 0 = return ()
        go j = do
            !(x :: Int) <- peek (castPtr ptr)
            poke (castPtr ptr) $! (x+1)
            !(x :: Int) <- peekByteOff ptr ccc
            pokeByteOff ptr ccc $! (x+1)
            go $! j - 1
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


-- test_mallocMemPool :: Int -> IO ()
-- test_mallocMemPool i = do
--     let go 0 = return ()
--         go j = do
--             ptr <- MemPool.allocPtr
--             go (j - 1)
--     MemPool.evalMemoryPoolT 8 1024 $ go i



test_malloc1 :: Int -> IO ()
test_malloc1 i = do
    let go 0 = return ()
        go j = do
            (ptr :: Ptr()) <- Ptr.mallocBytes 1
            Ptr.free ptr
            go (j - 1)
    go i

test_malloc100 :: Int -> IO ()
test_malloc100 i = do
    let go 0 = return ()
        go j = do
            (ptr :: Ptr()) <- Ptr.mallocBytes 100
            Ptr.free ptr
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




data TwoPart = TwoPart {-# UNBOX #-} !Int {-# UNBOX #-} !Int deriving Show

chunkSize :: Int
chunkSize = sizeOf' @Int
instance Storable TwoPart where
    sizeOf    (~_) = 2 * chunkSize ; {-# INLINE sizeOf    #-}
    alignment (~_) = chunkSize     ; {-# INLINE alignment #-}
    peek !ptr = TwoPart <$> peek (castPtr ptr) <*> peekByteOff ptr chunkSize ; {-# INLINE peek #-}
    poke !ptr = \(TwoPart !b !n) -> poke (castPtr ptr) b >> pokeByteOff ptr chunkSize n ; {-# INLINE poke #-}



test_singleStorable :: Int -> IO ()
test_singleStorable i = do
    ptr <- Ptr.new (TwoPart 0 0)
    let go 0 = return ()
        go j = do
            (TwoPart x y) <- peek ptr
            poke ptr (TwoPart (x+1) (y+1))
            go (j - 1)
    go i
    Ptr.free ptr


test_partialStorable :: Int -> IO ()
test_partialStorable i = do
    ptr <- Ptr.new (TwoPart 0 0)
    let go 0 = return ()
        go j = do
            (x :: Int) <- peek (castPtr ptr)
            (y :: Int) <- peekByteOff ptr chunkSize
            poke (castPtr ptr) (x + 1)
            pokeByteOff ptr chunkSize (y + 1)
            go (j - 1)
    go i
    Ptr.free ptr


vn :: Int
vn = 20

test_VectorCreation :: Int -> IO ()
test_VectorCreation i = do
    let go 0 = return ()
        go j = do
            (v :: Vector.IOVector Int) <- Vector.new vn
            go' v (vn - 1)
            go (j - 1)

        go' :: Vector.IOVector Int -> Int -> IO ()
        go' v j = go'' j where
            go'' 0 = return ()
            go'' j = do
                Vector.unsafeWrite v j j
                go'' (j - 1)
        {-# INLINE go' #-}
    go i

test_ListCreation :: Int -> IO ()
test_ListCreation i = do
    let go 0 = return ()
        go j = do
            evaluate =<< go' vn
            go (j - 1)

        go' 0 = return []
        go' j = (j :) <$> go' (j - 1)
    go i


test_VectorCreationHardcoded :: Int -> IO ()
test_VectorCreationHardcoded i = do
    let go 0 = return ()
        go j = do
            let (v :: V.Vector Int) = V.fromList [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
            evaluate v
            go (j - 1)
    go i

test_ListCreationHardcoded :: Int -> IO ()
test_ListCreationHardcoded i = do
    let go 0 = return ()
        go j = do
            let (lst :: [Int]) = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
            evaluate lst
            go (j - 1)
    go i
