module Luna.Test.Build.Dependency.ParserTestUtils where

import Prologue

import Text.Megaparsec      as P
import Text.Megaparsec.Text as P

import Test.Hspec
import Test.Hspec.Megaparsec

shouldParseTo :: (Eq a, Show a) => Text -> P.Parser a -> a -> Expectation
shouldParseTo input parser result = do
    r <- mapLeft displayException <$> pure (runParser parser "" input)
    r `shouldBe` (Right result)

shouldFailToParse :: (Eq a, Show a) => Parser a -> Text -> Expectation
shouldFailToParse parser input = parse parser "" `shouldFailOn` input

