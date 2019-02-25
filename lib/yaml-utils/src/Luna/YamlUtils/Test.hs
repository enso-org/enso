{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.YamlUtils.Test where

import Prologue

import qualified Data.ByteString as ByteString
import qualified Data.Yaml       as Yaml

import Data.ByteString (ByteString)
import Test.Hspec      (Expectation, shouldBe)

--------------------------------------------------------------
-- === Utilities for Testing Yaml Encoding and Decoding === --
--------------------------------------------------------------

shouldDecodeAs :: forall a . (Eq a, Show a, Yaml.FromJSON a) => ByteString
               -> Maybe a -> Expectation
shouldDecodeAs bStr mConfig = Yaml.decodeThrow bStr `shouldBe` mConfig

shouldNotDecode :: forall a . (Eq a, Show a, Yaml.FromJSON a) => ByteString
                -> Expectation
shouldNotDecode bStr = Yaml.decodeThrow bStr `shouldBe` (Nothing :: Maybe a)

shouldGenerate :: forall a . (Eq a, Show a, Yaml.ToJSON a) => a -> ByteString
               -> Expectation
shouldGenerate input bStr = Yaml.encode input `shouldBe` strippedExpected where
    newline          = ByteString.head "\n" -- safe as is a literal
    strippedExpected = case ByteString.uncons bStr of
        Just (head, rest) -> if head == newline then rest else bStr
        Nothing           -> bStr

