module Luna.CI where

import Prelude

import Data.Maybe         (fromJust, isJust)
import System.Directory   (createDirectoryIfMissing)
import System.Environment (lookupEnv)
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
            circleTestPath <- fmap fromJust $ lookupEnv "CIRCLE_TEST_REPORTS"
            let testPath = circleTestPath </> "hspec"
            createDirectoryIfMissing True testPath
            return $ defaultConfig { configFormatter = Just xmlFormatter
                                   , configOutputFile = Right $ testPath </> "output.xml" }
        else return defaultConfig

    hspecWith config spec

