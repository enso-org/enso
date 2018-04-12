module Main where

import Control.Monad (unless)
import System.Exit (exitFailure)
import System.IO (withFile, IOMode(..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Runner
import Test.Hspec.Formatters.Jenkins (xmlFormatter)

import Example


main :: IO ()
main = do
  -- hspec spec
  summary <- withFile "results.xml" WriteMode $ \h -> do
    let c = defaultConfig
          { configFormatter = xmlFormatter
          , configHandle = h
          }
    hspecWith c spec
  unless (summaryFailures summary == 0) $
    exitFailure

spec :: Spec
spec = do
  describe "sub" $ do
    it "subtracts" $
      sub 1 5 == 1 - 5
  describe "add" $ do
    prop "truth" $ True
    prop "adds" $ \n m -> add n m == n + m
  describe "exn&" $ do
    describe "<desc>" $ do
      it "throws IOException" $
        exn `shouldThrow` anyIOException
  describe "foo" $ do
    it "bar" $ do
      pendingWith "baz"
    it "hoge" $ do
      pending
