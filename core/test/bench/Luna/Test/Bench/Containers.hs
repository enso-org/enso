{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Test.Bench.Containers where

import Prologue

import qualified Control.Monad.State.Strict         as State
import qualified Criterion.Main                     as Criterion
import qualified Data.AutoVector.Mutable.Storable   as Vector
import qualified Data.Generics.Traversable          as GTraversable
import qualified Data.Generics.Traversable.Deriving as GTraversable
import qualified Data.Generics.Traversable2         as GTraversable2
import qualified Data.Generics.Traversable2.Class   as GTraversable2
import qualified Data.PtrSet.Mutable                as PtrSet
import qualified Data.Storable                      as Struct
import qualified Data.VectorSet.Mutable.Storable    as VectorSet
import qualified Foreign.Marshal.Alloc              as Ptr
import qualified Foreign.Marshal.Utils              as Ptr
import qualified Foreign.Ptr                        as Ptr
import qualified Foreign.Storable                   as Storable

import Control.DeepSeq            (force)
import Control.Exception          (evaluate)
import Control.Monad.State.Strict (State)
import Criterion.Main             (bgroup, env)
import Criterion.Main             (Benchmark)
import Data.Generics.Traversable  (GTraversable)
import Data.Storable              (type (-::))
import Luna.Test.Bench.IR         (Bench (Bench), bench)
import Unsafe.Coerce              (unsafeCoerce)


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

rw_ptr_rel_y :: Bench
rw_ptr_rel_y = Bench "rw ptr rel Y" $ \i -> do
    let isize = Storable.sizeOf (undefined :: Int)
    !ptr0 <- Ptr.mallocBytes (isize * 1000000)
    let off  = isize * 1
        ptr1 = ptr0 `Ptr.plusPtr` off
    Storable.poke ptr0 (0 :: Int)
    Storable.poke ptr1 (0 :: Int)
    _ <- Ptr.mallocBytes (isize)
    _ <- Ptr.mallocBytes (10*isize)
    _ <- Ptr.mallocBytes (100*isize)
    let go !0 = pure ()
        go !j = do
            !off1 <- Storable.peek ptr0
            if off1 == 0 then do
                !(x :: Int) <- Storable.peek $! ptr0 `Ptr.plusPtr` isize
                Storable.poke ptr1 $! x + 1
            else do
                let ptr1 = ptr0 `Ptr.plusPtr` off1
                !(x :: Int) <- Storable.peek ptr1
                Storable.poke ptr1 $! x + 1
            go $! j - 1
    go i
    Ptr.free ptr0
{-# NOINLINE rw_ptr_rel_y #-}

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




data Foo
   = FooA !Int !Int !Int !Int !Int !Int !Int !String !String !Float !Double
   | FooB !String !Int !Int !Int !Int !Int !Int !Int !String !String !Float
   deriving (Show, Generic)
GTraversable.derive ''Foo

instance
    ( GTraversable2.TraverseElem t Int
    , GTraversable2.TraverseElem t String
    , GTraversable2.TraverseElem t Float
    , GTraversable2.TraverseElem t Double
    ) => GTraversable2.Traversable t Foo where
    traverse = \case
        (FooA !t1 !t2 !t3 !t4 !t5 !t6 !t7 !t8 !t9 !t10 !t11) -> FooA <$> GTraversable2.traverseElem @t t1 <*> GTraversable2.traverseElem @t t2 <*> GTraversable2.traverseElem @t t3 <*> GTraversable2.traverseElem @t t4 <*> GTraversable2.traverseElem @t t5 <*> GTraversable2.traverseElem @t t6 <*> GTraversable2.traverseElem @t t7 <*> GTraversable2.traverseElem @t t8 <*> GTraversable2.traverseElem @t t9 <*> GTraversable2.traverseElem @t t10 <*> GTraversable2.traverseElem @t t11
        (FooB !t1 !t2 !t3 !t4 !t5 !t6 !t7 !t8 !t9 !t10 !t11) -> FooB <$> GTraversable2.traverseElem @t t1 <*> GTraversable2.traverseElem @t t2 <*> GTraversable2.traverseElem @t t3 <*> GTraversable2.traverseElem @t t4 <*> GTraversable2.traverseElem @t t5 <*> GTraversable2.traverseElem @t t6 <*> GTraversable2.traverseElem @t t7 <*> GTraversable2.traverseElem @t t8 <*> GTraversable2.traverseElem @t t9 <*> GTraversable2.traverseElem @t t10 <*> GTraversable2.traverseElem @t t11
    {-# INLINE traverse #-}

instance NFData Foo

data XX (m :: Type -> Type)

type instance GTraversable2.Result (XX m) = m
type instance GTraversable2.Result (YYF ctx m) = State.StateT (YYF ctx m) m

newtype YYF (ctx :: (Type -> Type) -> Type -> Constraint) m = YYF (∀ a. ctx m a => a -> m a)


class FooC m a where
    fooc :: a -> m a

instance Applicative m => FooC m Int    where fooc = \a -> pure (a + 1)  ; {-# INLINE fooc #-}
instance Applicative m => FooC m String where fooc = \a -> pure ('!' : a) ; {-# INLINE fooc #-}
instance Applicative m => FooC m Double where fooc = \a -> pure (a * 2)  ; {-# INLINE fooc #-}
instance Applicative m => FooC m Float  where fooc = \a -> pure (a / 2)  ; {-# INLINE fooc #-}

instance {-# OVERLAPPING #-} FooC m a
      => GTraversable2.TraverseElem (XX m) a where
    traverseElem = fooc ; {-# INLINE traverseElem #-}

instance {-# OVERLAPPING #-} (ctx m a, Monad m)
      => GTraversable2.TraverseElem (YYF ctx m) a where
    traverseElem = \a -> do
        YYF f <- State.get
        lift $ f a
    {-# INLINE traverseElem #-}

gt_bench :: Int -> Criterion.Benchmark
gt_bench j = benchEnv "gt" (pure $ FooA 1 2 3 4 5 6 7 "asd" "fds" 0.8 11.4)
             $ \a -> let
    go 0 = pure ()
    go i = do
        a' <- GTraversable.gtraverse @(FooC IO) fooc a
        evaluate $ force a'
        go (i - 1)
    in go j
{-# NOINLINE gt_bench #-}

gtx_bench :: Int -> Criterion.Benchmark
gtx_bench j = benchEnv "gtx" (pure $ FooA 1 2 3 4 5 6 7 "asd" "fds" 0.8 11.4)
             $ \a -> let
    go 0 = pure ()
    go i = do
        a' <- GTraversable2.traverse @(XX IO) a
        evaluate $ force a'
        go (i - 1)
    in go j
{-# NOINLINE gtx_bench #-}

gtx2_bench :: Int -> Criterion.Benchmark
gtx2_bench j = benchEnv "gtx2" (pure $ FooA 1 2 3 4 5 6 7 "asd" "fds" 0.8 11.4)
             $ \a -> let
    go 0 = pure ()
    go i = do
        a' <- State.evalStateT (GTraversable2.traverse @(YYF FooC IO) a) (YYF @FooC fooc)
        evaluate $ force a'
        go (i - 1)
    in go j
{-# NOINLINE gtx2_bench #-}


gt_fold_bench :: Int -> Criterion.Benchmark
gt_fold_bench j = benchEnv "gtfold" (pure $ FooA 1 2 3 4 5 6 7 "asd" "fds" 0.8 11.4)
             $ \a -> let
    go 0 = pure ()
    go i = do
        let x = GTraversable2.foldr @SumEls (0 :: Int) a
        evaluate $ force x
        go (i - 1)
    in go j
{-# NOINLINE gt_fold_bench #-}


gt_fold_bench2 :: Int -> Criterion.Benchmark
gt_fold_bench2 j = benchEnv "gtfold2" (pure $ FooA 1 2 3 4 5 6 7 "asd" "fds" 0.8 11.4)
             $ \a -> let
    go 0 = pure ()
    go i = do
        x <- GTraversable2.foldlM @SumEls (0 :: Int) a
        evaluate $ force x
        go (i - 1)
    in go j
{-# NOINLINE gt_fold_bench2 #-}



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
        [ gt_fold_bench  1000000
        , gt_fold_bench2 1000000
        , gt_bench   1
        , gtx_bench  1
        , gtx2_bench 1
        , ptrSet_tree2
        , vectorSet_insert2
        , "create" $ bench 5 <$>
            [ rw_ptr_ptr
            , rw_ptr_rel_x
            , rw_ptr_rel_y
            ]
        ]
      ]



data SumEls

instance {-# OVERLAPPABLE #-} GTraversable2.FoldElemM SumEls a IO Int
instance GTraversable2.FoldElemM SumEls Int IO Int where
    foldElemM a s = pure $ a + s
    {-# INLINE foldElemM #-}

instance {-# OVERLAPPABLE #-} GTraversable2.FoldElem SumEls a Int
instance GTraversable2.FoldElem SumEls Int Int where
    foldElem a s = a + s
    {-# INLINE foldElem #-}

data GatherEls
instance {-# OVERLAPPABLE #-} GTraversable2.FoldElemM GatherEls a IO [Int]
instance GTraversable2.FoldElemM GatherEls Int IO [Int] where
    foldElemM a s = pure $ a : s
    {-# INLINE foldElemM #-}

instance {-# OVERLAPPABLE #-} GTraversable2.FoldElem GatherEls a [Int]
instance GTraversable2.FoldElem GatherEls Int [Int] where
    foldElem a s = a : s
    {-# INLINE foldElem #-}


test :: IO ()
test = do
    print "test"
    let foo = FooA 1 2 3 4 5 6 7 "asd" "fds" 0.8 11.4
    x2 <- GTraversable2.foldlM @GatherEls (mempty :: [Int]) foo
    print x2
    print "---"
    print $ GTraversable2.foldl @GatherEls (mempty :: [Int]) foo
    -- print $ GTraversable2.toListByType @Int foo
    print "/ test"

main :: IO ()
main = do
    test
    benchmarks
