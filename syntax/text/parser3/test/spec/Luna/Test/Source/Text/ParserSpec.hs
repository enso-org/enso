{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Luna.Test.Source.Text.ParserSpec where

import Prologue
import Test.Hspec.Expectations.Lifted

import Test.Hspec      (Arg, Example, Expectation, Spec, describe, it)
import Test.Hspec.Core (SpecM)


debugSpec :: Spec
debugSpec = describe "error" $ it "x" $ do
    pure () :: IO ()

    True `shouldBe` False



spec :: Spec
spec = do
    debugSpec

