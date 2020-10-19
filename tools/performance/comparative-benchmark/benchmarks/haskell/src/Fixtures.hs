module Fixtures where

import Prelude

import qualified Data.Map.Strict as Map

import Control.Monad (foldM)
import Data.Int (Int64)
import Data.List (foldl')
import System.Random (randomRIO)


------------------
-- === List === --
------------------

data List a = Cons a (List a) | Nil deriving (Show)

--------------------------
-- === Input Values === --
--------------------------

genList :: Int64 -> List Int64
genList = \(!list) -> go Nil list where
    go :: List Int64 -> Int64 -> List Int64
    go acc i = if i == 0 then acc else let
        !c1 = Cons i acc
        !i1 = i - 1
        in go c1 i1

millionElementList :: List Int64
millionElementList = genList 1000000

hundredMillion :: Int64
hundredMillion = 100000000

tenThousand :: Integer
tenThousand = 10000

----------------------
-- === Fixtures === --
----------------------

sumTCO :: Int64 -> Int64
sumTCO sumTo = let
    summator accumulator current =
        if current == 0 then accumulator
        else summator (accumulator + current) (current - 1)
    in summator 0 sumTo

sumList :: List Int64 -> Int64
sumList list = let
    summator accumulator list = case list of
        Cons x xs -> summator (accumulator + x) xs
        Nil       -> accumulator
    in summator 0 list

reverseList :: List Int64 -> List Int64
reverseList list = let
    reverser accumulator lst = case lst of
        Cons x xs -> reverser (Cons x accumulator) xs
        Nil       -> accumulator
    in reverser Nil list

sumListLeftFold :: List Int64 -> Int64
sumListLeftFold = myFoldl (+) 0

myFoldl :: (t -> t -> t) -> t -> List t -> t
myFoldl _ z Nil         = z
myFoldl f z (Cons x xs) = let z' = z `f` x
                    in seq z' $ myFoldl f z' xs

buildMap :: Integer -> Map.Map Integer Integer
buildMap n = foldl' (\m i -> Map.insert i i m) Map.empty [0..n]

buildRandomMap :: Integer -> IO (Map.Map Integer Integer)
buildRandomMap n = foldM (\m i -> fmap (\key -> Map.insert key i m) $ randomRIO (0, 10000)) Map.empty [0..n]
