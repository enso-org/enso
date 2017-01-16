module Main where

import Data.Maybe (isJust)
import System.Environment (lookupEnv)

import Test.Hspec.Runner
import Test.Hspec.Formatters.Jenkins
import qualified Spec

import Prelude

main :: IO ()
main = do
    buildInCI <- isJust <$> lookupEnv "CIRCLECI"
    let config = if buildInCI then defaultConfig {configFormatter = Just xmlFormatter}
                              else defaultConfig

    hspecWith config Spec.spec
