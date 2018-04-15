module Luna.Test.Build.Dependency.ParserTestUtils where

import Prologue

import Luna.Build.Dependency.ParserUtils (Parser)
import Test.Hspec                        (Expectation, shouldBe)
import Test.Hspec.Megaparsec             (shouldFailOn)
import Text.Megaparsec                   (runParser, parse)

shouldParseTo :: (Eq a, Show a) => Text -> Parser a -> a -> Expectation
shouldParseTo input parser result = do
    r <- mapLeft show <$> pure (runParser parser "" input)
    r `shouldBe` Right result

shouldFailToParse :: (Eq a, Show a) => Parser a -> Text -> Expectation
shouldFailToParse parser input = parse parser "" `shouldFailOn` input

