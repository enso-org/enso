{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Luna.CI where

import Prelude

import System.Environment as Environment

import Data.Maybe         (fromMaybe, fromJust, isJust)
import System.Directory   (createDirectoryIfMissing)
import System.FilePath    ((</>))

import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.Formatters.Jenkins

main :: Spec -> IO ()
main spec = do
    buildInCI <- isJust <$> lookupEnv "CIRCLECI"

    config <-
        if buildInCI then do
            -- if in CI, CIRCLE_TEST_REPORTS is always defined
            envResult <- Environment.lookupEnv "CIRCLE_TEST_REPORTS"
            let circleTestPath = fromMaybe "" envResult

            if null circleTestPath then
                pure defaultConfig
            else do
                let testPath = circleTestPath </> "hspec"
                createDirectoryIfMissing True testPath
                pure $ defaultConfig { configFormatter = Just xmlFormatter
                                     , configOutputFile = Right
                                        $ testPath </> "output.xml" }
        else pure defaultConfig

    hspecWith config spec

