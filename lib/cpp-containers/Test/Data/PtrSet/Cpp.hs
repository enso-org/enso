{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE BangPatterns #-}

module Test.Data.PtrSet.Cpp where

-- import Prologue

-- import qualified Data.IntSet     as IntSet
-- import qualified Data.PtrSet.Cpp as PtrSet

-- import Data.List             (unfoldr)
-- import Data.PtrSet.Cpp       (UnmanagedPtrSet)
-- import Foreign.ForeignPtr
-- import Foreign.Marshal.Alloc
-- import Foreign.Marshal.Array
-- import Foreign.Ptr
-- import Foreign.Ptr.Utils
-- import System.Random


-- ---------------------------
-- -- === Foreign calls === --
-- ---------------------------

-- foreign import ccall unsafe "c_set_test_insert_and_lookup"
--     c_set_testInsertAndLookup :: Ptr SomePtr -> Int -> Ptr SomePtr -> Int
--                               -> UnmanagedPtrSet -> IO Int

-- foreign import ccall unsafe "c_set_test_insert_lookup_ordered"
--     c_set_testInsertLookupOrdered :: Int -> IO Int

-- foreign import ccall unsafe "c_set_identity"
--     c_set_identity :: Ptr SomePtr -> Int -> IO Int


-- -------------------
-- -- === Utils === --
-- -------------------

-- randomList :: Int -> [Int]
-- randomList n = take n . unfoldr (Just . random) $ mkStdGen 123456



---------------------------
-- === Testing logic === --
---------------------------

-- testInsertAndLookupIntSet :: Int -> IO Int
-- testInsertAndLookupIntSet n = do
--     let rndm  = randomList n
--         rndm2 = randomList (n `div` 2)
--         go  !s ((!x):xs) = go (IntSet.insert x s) xs
--         go  !s []        = s
--
--         go2 !s !sm ((!x):xs) = go2 s (sm + (if IntSet.member x s then 1 else 0))
--                                xs
--         go2 !s !sm []        = sm
--
--         s1   = go IntSet.empty rndm
--         suma = go2 s1 0 rndm2
--     pure suma
--
-- testInsertAndLookupCIntSet :: Int -> IO Int
-- testInsertAndLookupCIntSet n = do
--     -- create a set based on a random list
--     let rndm  = randomList n
--         rndm2 = randomList (n `div` 2)
--     s  <- CIntSet.new @IntSet
--     let go  ((!x):xs) = CIntSet.insert s x >> go xs
--         go  []        = pure ()
--
--         go2 !sm ((!x):xs) = do
--                 mem <- CIntSet.member s x
--                 go2 (sm + (if mem then 1 else 0)) xs
--         go2 !sm [] = pure sm
--
--     go rndm
--     go2 0 rndm2
--
-- testInsertLookupOrderedCIntSet :: Int -> IO Int
-- testInsertLookupOrderedCIntSet n = do
--     s <- CIntSet.new @IntSet
--     let go !x = when_ (x < n) $ CIntSet.insert s x >> go (x + 1)
--     go 0
--
--     let go2 !x !sm = if x < n
--         then do
--             m <- CIntSet.member s (n - 1)
--             go2 (x + 1) $ sm + (if m then 1 else 0)
--         else pure sm
--
--     go2 0 0
--
-- testInsertAndLookupForeignSet :: Int -> IO Int
-- testInsertAndLookupForeignSet n = do
--     let rndm  = randomList n
--         rndm2 = randomList (n `div` 2)
--     s <- CIntSet.new @IntSet
--     CIntSet.with s $ \ptrSet ->
--         withArrayLen rndm  $ \n1 ptrInsert ->
--         withArrayLen rndm2 $ \n2 ptrIdx    ->
--             c_set_testInsertAndLookup ptrInsert n1 ptrIdx n2 ptrSet
--
-- testInsertLookupOrderedForeignSet :: Int -> IO Int
-- testInsertLookupOrderedForeignSet = c_set_testInsertLookupOrdered
--
-- testWithArrayLen :: Int -> IO Int
-- testWithArrayLen n = do
--     let rndm  = randomList n
--         rndm2 = randomList (n `div` 2)
--
--     withArrayLen rndm $ flip c_set_identity
