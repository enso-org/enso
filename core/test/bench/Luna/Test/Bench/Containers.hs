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

import Criterion.Main     (bgroup, env)
import Criterion.Main     (Benchmark)
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


benchEnv :: NFData env => String -> IO env -> (env -> IO ()) -> Benchmark
benchEnv name env f = Criterion.env env
    $ \ (~a) -> Criterion.bench name $ Criterion.whnfIO $ f a
{-# INLINE benchEnv #-}




pxels :: [Ptr.Ptr ()]
pxels = unsafeCoerce ([7, 3, 5, 12, 18, 2, 23, 14, 1, 3] :: [Int])
{-# INLINE pxels #-}

ptrSet_tree2 :: Criterion.Benchmark
ptrSet_tree2 = benchEnv "stdSet" PtrSet.new
             $ \ (s :: PtrSet.UnmanagedPtrSet (Ptr.Ptr ())) -> do
    mapM_ (PtrSet.insert s) pxels
    mapM_ (PtrSet.delete s) pxels
{-# NOINLINE ptrSet_tree2 #-}

vectorSet_insert2 :: Criterion.Benchmark
vectorSet_insert2 = benchEnv "vectorSet" (VectorSet.VectorSet <$> Vector.new 32)
             $ \ (s :: VectorSet.VectorSet (Ptr.Ptr ())) -> do
    mapM_ (VectorSet.insert s) pxels
    mapM_ (VectorSet.remove s) pxels
{-# NOINLINE vectorSet_insert2 #-}



benchmarks :: IO ()
benchmarks = do
    Criterion.defaultMain
      [ "containers"
        [ ptrSet_tree2
        , vectorSet_insert2
        ]
      ]


main :: IO ()
main = benchmarks
