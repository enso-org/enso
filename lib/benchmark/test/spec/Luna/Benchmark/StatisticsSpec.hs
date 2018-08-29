module Luna.Benchmark.StatisticsSpec where

import Prologue

import qualified Luna.Benchmark.Statistics as Statistics

import Data.ByteString           ( ByteString )
import Luna.Benchmark.Statistics ( Statistics )
import Luna.YamlUtils.Test       ( shouldDecodeAs, shouldGenerate )
import Test.Hspec                ( Spec, describe, it )



-----------------------
-- === Test Data === --
-----------------------

statsEx1 :: Statistics
statsEx1 = (def @Statistics) & Statistics.timeInfo . Statistics.minTime .~ 100

statsEx1Yaml :: ByteString
statsEx1Yaml = [qqStr|
location-name: ''
tick-info:
  std-ticks: 0
  min-ticks: 18446744073709551615
  max-ticks: 0
  tick-counts: []
  avg-ticks: 0
time-info:
  max-time: 0
  avg-time: 0
  std-time: 0
  min-time: 100
  times: []
mem-info:
  min-mem:
    live-bytes: 9223372036854775807
    max-bytes: 9223372036854775807
    actual-bytes: 9223372036854775807
    num-g-cs: 9223372036854775807
  mem-vals: []
  std-mem:
    live-bytes: 0
    max-bytes: 0
    actual-bytes: 0
    num-g-cs: 0
  max-mem:
    live-bytes: 0
    max-bytes: 0
    actual-bytes: 0
    num-g-cs: 0
  avg-mem:
    live-bytes: 0
    max-bytes: 0
    actual-bytes: 0
    num-g-cs: 0
source-locations: []
|]

statsList :: [Statistics]
statsList =
    [ (def @Statistics) & Statistics.timeInfo . Statistics.minTime .~ 100 ]

statsListYaml :: ByteString
statsListYaml = [qqStr|
- location-name: ''
  tick-info:
    std-ticks: 0
    min-ticks: 18446744073709551615
    max-ticks: 0
    tick-counts: []
    avg-ticks: 0
  time-info:
    max-time: 0
    avg-time: 0
    std-time: 0
    min-time: 100
    times: []
  mem-info:
    min-mem:
      live-bytes: 9223372036854775807
      max-bytes: 9223372036854775807
      actual-bytes: 9223372036854775807
      num-g-cs: 9223372036854775807
    mem-vals: []
    std-mem:
      live-bytes: 0
      max-bytes: 0
      actual-bytes: 0
      num-g-cs: 0
    max-mem:
      live-bytes: 0
      max-bytes: 0
      actual-bytes: 0
      num-g-cs: 0
    avg-mem:
      live-bytes: 0
      max-bytes: 0
      actual-bytes: 0
      num-g-cs: 0
  source-locations: []
|]



-------------------
-- === Tests === --
-------------------

spec :: Spec
spec = do
    describe "Parsing of Yaml Performance Records" $ do
        it "Succeeds when defaulted"
            $ statsEx1Yaml `shouldDecodeAs` (Just statsEx1)

    describe "Generation of Yaml Performance Records" $ do
        it "Succeeds when defaulted" $ statsEx1 `shouldGenerate` statsEx1Yaml

    describe "Lists of statistics" $ do
        it "Encode successfully" $ statsList `shouldGenerate` statsListYaml
        it "Decode successfully"
            $ statsListYaml `shouldDecodeAs` (Just statsList)

