{-# LANGUAGE OverloadedStrings #-}

module Luna.Test.Bench.Containers where

import Prologue

import qualified Criterion.Main                   as Criterion
import qualified Data.AutoVector.Mutable.Storable as Vector
import qualified Data.PtrSet.Mutable              as PtrSet
import qualified Data.Storable                    as Struct
import qualified Data.VectorSet.Mutable.Storable  as VectorSet
import qualified Foreign.Marshal.Alloc            as Ptr
import qualified Foreign.Marshal.Utils            as Ptr
import qualified Foreign.Ptr                      as Ptr
import qualified Foreign.Storable                 as Storable

import Data.Storable      (type (-::))
import Luna.Test.Bench.IR (Bench (Bench), bench)
import Unsafe.Coerce      (unsafeCoerce)


newtype X = X (Struct.Struct '["foo" -:: Int])
makeLenses ''X

instance Struct.IsStruct X

foreign import ccall unsafe "c_ptr_rwloop"
    c_ptr_rwloop :: Int -> Int -> IO Int

readWrite_cptr :: Bench
readWrite_cptr = Bench "cptr" $ void . c_ptr_rwloop 1
{-# INLINE readWrite_cptr #-}


foo :: Struct.FieldRef "foo"
foo = Struct.field @"foo" ; {-# INLINE foo #-}

xcons :: Int -> IO X
xcons = Struct.construct @X ; {-# INLINE xcons #-}

rw_ptr :: Bench
rw_ptr = Bench "rw ptr" $ \i -> do
    !ptr <- Ptr.new (0 :: Int)
    let go !0 = pure ()
        go !j = do
            !x <- Storable.peek ptr
            Storable.poke ptr $! x + 1
            go $! j - 1
    go i
    Ptr.free ptr
{-# NOINLINE rw_ptr #-}

rw_ptr_rel :: Bench
rw_ptr_rel = Bench "rw ptr rel" $ \i -> do
    !ptr <- Ptr.mallocBytes (2 * Storable.sizeOf (undefined :: Int))
    Storable.poke ptr (Storable.sizeOf (undefined :: Int))
    let go !0 = pure ()
        go !j = do
            !off <- Storable.peek ptr
            let ptr' = ptr `Ptr.plusPtr` off
            !(x :: Int) <- Storable.peek ptr'
            Storable.poke ptr' $! x + 1
            go $! j - 1
    go i
    Ptr.free ptr
{-# NOINLINE rw_ptr_rel #-}

rw_ptr_rel_2 :: Bench
rw_ptr_rel_2 = Bench "rw ptr rel 2" $ \i -> do
    let isize = Storable.sizeOf (undefined :: Int)
    !ptr <- Ptr.mallocBytes (3 * isize)
    Storable.poke ptr isize
    Storable.poke (ptr `Ptr.plusPtr` isize) (2 * isize)
    let go !0 = pure ()
        go !j = do
            !off1 <- Storable.peek ptr
            let ptr1 = ptr `Ptr.plusPtr` off1
            !off2 <- Storable.peek ptr1
            let ptr2 = ptr `Ptr.plusPtr` off2
            !(x :: Int) <- Storable.peek ptr2
            Storable.poke ptr2 $! x + 1
            go $! j - 1
    go i
    Ptr.free ptr
{-# NOINLINE rw_ptr_rel_2 #-}

rw_ptr_rel_3 :: Bench
rw_ptr_rel_3 = Bench "rw ptr rel 3" $ \i -> do
    let isize = Storable.sizeOf (undefined :: Int)
    !ptr <- Ptr.mallocBytes (4 * isize)
    Storable.poke ptr isize
    Storable.poke (ptr `Ptr.plusPtr` isize)       (2 * isize)
    Storable.poke (ptr `Ptr.plusPtr` (2 * isize)) (3 * isize)
    let go !0 = pure ()
        go !j = do
            !off1 <- Storable.peek ptr
            let ptr1 = ptr `Ptr.plusPtr` off1
            !off2 <- Storable.peek ptr1
            let ptr2 = ptr `Ptr.plusPtr` off2
            !off3 <- Storable.peek ptr2
            let ptr3 = ptr `Ptr.plusPtr` off3
            !(x :: Int) <- Storable.peek ptr3
            Storable.poke ptr3 $! x + 1
            go $! j - 1
    go i
    Ptr.free ptr
{-# NOINLINE rw_ptr_rel_3 #-}

rw_ptr_rel_4 :: Bench
rw_ptr_rel_4 = Bench "rw ptr rel 4" $ \i -> do
    let isize = Storable.sizeOf (undefined :: Int)
    !ptr <- Ptr.mallocBytes (5 * isize)
    Storable.poke ptr isize
    Storable.poke (ptr `Ptr.plusPtr` isize)       (2 * isize)
    Storable.poke (ptr `Ptr.plusPtr` (2 * isize)) (3 * isize)
    Storable.poke (ptr `Ptr.plusPtr` (3 * isize)) (4 * isize)
    let go !0 = pure ()
        go !j = do
            !off1 <- Storable.peek ptr
            let ptr1 = ptr `Ptr.plusPtr` off1
            !off2 <- Storable.peek ptr1
            let ptr2 = ptr `Ptr.plusPtr` off2
            !off3 <- Storable.peek ptr2
            let ptr3 = ptr `Ptr.plusPtr` off3
            !off4 <- Storable.peek ptr3
            let ptr4 = ptr `Ptr.plusPtr` off4
            !(x :: Int) <- Storable.peek ptr4
            Storable.poke ptr4 $! x + 1
            go $! j - 1
    go i
    Ptr.free ptr
{-# NOINLINE rw_ptr_rel_4 #-}

rw_ptr_rel_5 :: Bench
rw_ptr_rel_5 = Bench "rw ptr rel 5" $ \i -> do
    let isize = Storable.sizeOf (undefined :: Int)
    !ptr <- Ptr.mallocBytes (6 * isize)
    Storable.poke ptr isize
    Storable.poke (ptr `Ptr.plusPtr` isize)       (2 * isize)
    Storable.poke (ptr `Ptr.plusPtr` (2 * isize)) (3 * isize)
    Storable.poke (ptr `Ptr.plusPtr` (3 * isize)) (4 * isize)
    Storable.poke (ptr `Ptr.plusPtr` (4 * isize)) (5 * isize)
    let go !0 = pure ()
        go !j = do
            !off1 <- Storable.peek ptr
            let ptr1 = ptr `Ptr.plusPtr` off1
            !off2 <- Storable.peek ptr1
            let ptr2 = ptr `Ptr.plusPtr` off2
            !off3 <- Storable.peek ptr2
            let ptr3 = ptr `Ptr.plusPtr` off3
            !off4 <- Storable.peek ptr3
            let ptr4 = ptr `Ptr.plusPtr` off4
            !off5 <- Storable.peek ptr4
            let ptr5 = ptr `Ptr.plusPtr` off5
            !(x :: Int) <- Storable.peek ptr5
            Storable.poke ptr5 $! x + 1
            go $! j - 1
    go i
    Ptr.free ptr
{-# NOINLINE rw_ptr_rel_5 #-}

rw_ptr_rel_2x :: Bench
rw_ptr_rel_2x = Bench "rw ptr rel 2x" $ \i -> do
    let isize = Storable.sizeOf (undefined :: Int)
    !ptr0 <- Ptr.mallocBytes isize
    _ <- Ptr.mallocBytes (isize)
    _ <- Ptr.mallocBytes (10*isize)
    _ <- Ptr.mallocBytes (100*isize)
    !d1 <- Ptr.mallocBytes (100000*isize)
    !ptr1 <- Ptr.mallocBytes isize
    _ <- Ptr.mallocBytes (isize)
    _ <- Ptr.mallocBytes (10*isize)
    _ <- Ptr.mallocBytes (100*isize)
    !d2 <- Ptr.mallocBytes (100000*isize)
    !ptr2 <- Ptr.mallocBytes isize
    -- print "======"
    -- print ptr0
    -- print ptr1
    -- print ptr2
    Storable.poke ptr0 (ptr1 `Ptr.minusPtr` ptr0)
    Storable.poke ptr1 (ptr2 `Ptr.minusPtr` ptr1)
    let go !0 = pure ()
        go !j = do
            !off1 <- Storable.peek ptr0
            let ptr1 = ptr0 `Ptr.plusPtr` off1
            !off2 <- Storable.peek ptr1
            let ptr2 = ptr1 `Ptr.plusPtr` off2
            !(x :: Int) <- Storable.peek ptr2
            Storable.poke ptr2 $! x + 1
            go $! j - 1
    go i
    Ptr.free ptr0
    Ptr.free ptr1
    Ptr.free ptr2
    Ptr.free d1
    Ptr.free d2
{-# NOINLINE rw_ptr_rel_2x #-}

rw_ptr_rel_1x :: Bench
rw_ptr_rel_1x = Bench "rw ptr rel 1x" $ \i -> do
    let isize = Storable.sizeOf (undefined :: Int)
    !ptr0 <- Ptr.mallocBytes isize
    _ <- Ptr.mallocBytes (isize)
    _ <- Ptr.mallocBytes (10*isize)
    _ <- Ptr.mallocBytes (100*isize)
    !d1 <- Ptr.mallocBytes (100000*isize)
    !ptr1 <- Ptr.mallocBytes isize
    Storable.poke ptr0 (ptr1 `Ptr.minusPtr` ptr0)
    let go !0 = pure ()
        go !j = do
            !off1 <- Storable.peek ptr0
            let ptr1 = ptr0 `Ptr.plusPtr` off1
            !(x :: Int) <- Storable.peek ptr1
            Storable.poke ptr1 $! x + 1
            go $! j - 1
    go i
    Ptr.free ptr0
    Ptr.free ptr1
{-# NOINLINE rw_ptr_rel_1x #-}







rw_ptr_rel_x :: Bench
rw_ptr_rel_x = Bench "rw ptr rel X" $ \i -> do
    let isize = Storable.sizeOf (undefined :: Int)
    !ptr0 <- Ptr.mallocBytes (isize * 1000000)
    let off  = isize * 1
        ptr1 = ptr0 `Ptr.plusPtr` off
    Storable.poke ptr0 off
    Storable.poke ptr1 (0 :: Int)
    _ <- Ptr.mallocBytes (isize)
    _ <- Ptr.mallocBytes (10*isize)
    _ <- Ptr.mallocBytes (100*isize)
    let go !0 = pure ()
        go !j = do
            !off1 <- Storable.peek ptr0
            let ptr1 = ptr0 `Ptr.plusPtr` off1
            !(x :: Int) <- Storable.peek ptr1
            Storable.poke ptr1 $! x + 1
            go $! j - 1
    go i
    Ptr.free ptr0
{-# NOINLINE rw_ptr_rel_x #-}



rw_ptr_ptr :: Bench
rw_ptr_ptr = Bench "rw ptr ptr" $ \i -> do
    let isize = Storable.sizeOf (undefined :: Int)
    !ptr0 <- Ptr.mallocBytes (isize * 1000000)
    let off  = isize * 1
        ptr1 = ptr0 `Ptr.plusPtr` off
    Storable.poke ptr0 (ptr0 `Ptr.plusPtr` off)
    Storable.poke ptr1 (0 :: Int)
    _ <- Ptr.mallocBytes (isize)
    _ <- Ptr.mallocBytes (10*isize)
    _ <- Ptr.mallocBytes (100*isize)
    let go !0 = pure ()
        go !j = do
            !ptr1 <- Storable.peek ptr0
            !(x :: Int) <- Storable.peek ptr1
            Storable.poke ptr1 $! x + 1
            go $! j - 1
    go i
    Ptr.free ptr0
{-# NOINLINE rw_ptr_ptr #-}










struct_rw_field :: Bench
struct_rw_field = Bench "rw field" $ \i -> do
    a <- xcons 5
    let go !0 = pure ()
        go !j = do
            !t <- Struct.readField foo a
            Struct.writeField foo a $! t + 1
            go $! j - 1
    go i
{-# NOINLINE struct_rw_field #-}


ptrSet_tree :: Bench
ptrSet_tree = Bench "ptrset tree" $ \i -> do
    (s :: PtrSet.UnmanagedPtrSet (Ptr.Ptr ())) <- PtrSet.new
    let go !0 = pure ()
        go !j = do
            PtrSet.insert s $ (unsafeCoerce (7  :: Int) :: Ptr.Ptr ())
            PtrSet.insert s $ (unsafeCoerce (3  :: Int) :: Ptr.Ptr ())
            -- PtrSet.insert s $ (unsafeCoerce (5  :: Int) :: Ptr.Ptr ())
            -- PtrSet.insert s $ (unsafeCoerce (12 :: Int) :: Ptr.Ptr ())
            -- PtrSet.insert s $ (unsafeCoerce (18 :: Int) :: Ptr.Ptr ())
            -- PtrSet.insert s $ (unsafeCoerce (2  :: Int) :: Ptr.Ptr ())
            -- PtrSet.insert s $ (unsafeCoerce (23 :: Int) :: Ptr.Ptr ())
            -- PtrSet.insert s $ (unsafeCoerce (14 :: Int) :: Ptr.Ptr ())
            -- PtrSet.insert s $ (unsafeCoerce (1  :: Int) :: Ptr.Ptr ())
            -- PtrSet.insert s $ (unsafeCoerce (3  :: Int) :: Ptr.Ptr ())
            PtrSet.delete s $ (unsafeCoerce (7  :: Int) :: Ptr.Ptr ())
            PtrSet.delete s $ (unsafeCoerce (3  :: Int) :: Ptr.Ptr ())
            -- PtrSet.delete s $ (unsafeCoerce (5  :: Int) :: Ptr.Ptr ())
            -- PtrSet.delete s $ (unsafeCoerce (12 :: Int) :: Ptr.Ptr ())
            -- PtrSet.delete s $ (unsafeCoerce (18 :: Int) :: Ptr.Ptr ())
            -- PtrSet.delete s $ (unsafeCoerce (2  :: Int) :: Ptr.Ptr ())
            -- PtrSet.delete s $ (unsafeCoerce (23 :: Int) :: Ptr.Ptr ())
            -- PtrSet.delete s $ (unsafeCoerce (14 :: Int) :: Ptr.Ptr ())
            -- PtrSet.delete s $ (unsafeCoerce (1  :: Int) :: Ptr.Ptr ())
            -- PtrSet.delete s $ (unsafeCoerce (3  :: Int) :: Ptr.Ptr ())
            go $! j - 1
    go i
{-# NOINLINE ptrSet_tree #-}

vectorSet_insert :: Bench
vectorSet_insert = Bench "VectorSet insert" $ \i -> do
    (v :: Vector.Vector (Ptr.Ptr ())) <- Vector.new 32
    let s = VectorSet.VectorSet v
    let go !0 = pure ()
        go !j = do
            -- Vector.insert v 0 1
            -- Vector.remove v 0
            -- VectorSet.insert s 1
            -- VectorSet.remove s 1

            VectorSet.insert s (unsafeCoerce (7  :: Int) :: Ptr.Ptr ())
            VectorSet.insert s (unsafeCoerce (3  :: Int) :: Ptr.Ptr ())
            -- VectorSet.insert s 5
            -- VectorSet.insert s 12
            -- VectorSet.insert s 18
            -- VectorSet.insert s 2
            -- VectorSet.insert s 23
            -- VectorSet.insert s 14
            -- VectorSet.insert s 1
            -- VectorSet.insert s 3

            VectorSet.remove s (unsafeCoerce (7  :: Int) :: Ptr.Ptr ())
            VectorSet.remove s (unsafeCoerce (3  :: Int) :: Ptr.Ptr ())
            -- VectorSet.remove s 5
            -- VectorSet.remove s 12
            -- VectorSet.remove s 18
            -- VectorSet.remove s 2
            -- VectorSet.remove s 23
            -- VectorSet.remove s 14
            -- VectorSet.remove s 1
            -- VectorSet.remove s 3
            go $! j - 1
    go i
{-# NOINLINE vectorSet_insert #-}

benchmarks :: IO ()
benchmarks = do
    Criterion.defaultMain
      [ "containers"
        [ "struct" $ bench 0 <$>
            -- [ readWrite_cptr
            -- , rw_ptr
            -- , struct_rw_field
            [ rw_ptr_ptr
            , rw_ptr_rel_x
            , rw_ptr_rel
            , rw_ptr_rel_2
            , rw_ptr_rel_1x
            , rw_ptr_rel_2x
            , rw_ptr_rel_3
            , rw_ptr_rel_4
            , rw_ptr_rel_5
            , ptrSet_tree
            , vectorSet_insert
            ]
        ]
      ]


main :: IO ()
main = benchmarks
