module Main where

import Prelude

import qualified Fixtures as Fixtures

import Criterion.Main

main :: IO ()
main = defaultMain
    [
        bench "sumTCO"          $ whnf Fixtures.sumTCO Fixtures.hundredMillion,
        bench "sumList"         $ whnf Fixtures.sumList Fixtures.millionElementList,
        bench "reverseList"     $ whnf Fixtures.reverseList Fixtures.millionElementList,
        bench "sumListLeftFold" $ whnf Fixtures.reverseList Fixtures.millionElementList
    ]
