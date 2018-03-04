{-# LANGUAGE BangPatterns #-}
module Test.Data.IntSet.Cpp where

import           Prelude

import qualified Data.IntSet     as IntSet
import qualified Data.IntSet.Cpp as CSet
import           Data.IntSet.Cpp (StdSet, RawSetPtr, withStdSet, mapStdSet)

import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.List     (unfoldr)
import System.Random


---------------------------
-- === Foreign calls === --
---------------------------

foreign import ccall unsafe "c_set_test_insert_and_lookup"
    c_set_testInsertAndLookup :: Ptr Int -> Int -> Ptr Int -> Int -> RawSetPtr -> IO Int

foreign import ccall unsafe "c_set_test_insert_lookup_ordered"
    c_set_testInsertLookupOrdered :: Int -> IO Int

foreign import ccall unsafe "c_set_identity"
    c_set_identity :: Ptr Int -> Int -> IO Int


-------------------
-- === Utils === --
-------------------

randomList :: Int -> [Int]
randomList n = take n . unfoldr (Just . random) $ mkStdGen 123456



---------------------------
-- === Testing logic === --
---------------------------

testInsertAndLookupIntSet :: Int -> IO Int
testInsertAndLookupIntSet n = do
    let rndm  = randomList n
        rndm2 = randomList (n `div` 2)
        go  !s !(x:xs) = go (IntSet.insert x s) xs
        go  !s []      = s

        go2 !s !sm !(x:xs) = go2 s (sm + (if IntSet.member x s then 1 else 0)) xs
        go2 !s !sm []      = sm

        s1   = go IntSet.empty rndm
        suma = go2 s1 0 rndm2
    return suma

testInsertAndLookupCSet :: Int -> IO Int
testInsertAndLookupCSet n = do
    -- create a set based on a random list
    let rndm  = randomList n
        rndm2 = randomList (n `div` 2)
    s  <- CSet.empty
    let go  !(x:xs) = (CSet.insert s x) >> go xs
        go  ![]     = return ()

        go2 !sm !(x:xs) = do
                mem <- CSet.member s x
                go2 (sm + (if mem then 1 else 0)) xs
        go2 !sm ![] = return sm

    go rndm
    go2 0 rndm2

testInsertLookupOrderedCSet :: Int -> IO Int
testInsertLookupOrderedCSet n = do
    s <- CSet.empty
    let go !x = if x < n then (CSet.insert s x) >> go (x + 1) else return ()
    go 0

    let go2 !x !sm = if x < n
        then do
            m <- CSet.member s (n - 1)
            go2 (x + 1) $ sm + (if m then 1 else 0)
        else return sm

    go2 0 0

testInsertAndLookupForeignSet :: Int -> IO Int
testInsertAndLookupForeignSet n = do
    let rndm  = randomList n
        rndm2 = randomList (n `div` 2)
    s <- CSet.empty
    withStdSet s (\ptrSet ->
        withArrayLen rndm  (\n1 ptrInsert ->
        withArrayLen rndm2 (\n2 ptrIdx    ->
            c_set_testInsertAndLookup ptrInsert n1 ptrIdx n2 ptrSet)))

testInsertLookupOrderedForeignSet :: Int -> IO Int
testInsertLookupOrderedForeignSet n = c_set_testInsertLookupOrdered n

testWithArrayLen :: Int -> IO Int
testWithArrayLen n = do
    let rndm  = randomList n
        rndm2 = randomList (n `div` 2)

    withArrayLen rndm (\n1 ptr ->
        c_set_identity ptr n1)
