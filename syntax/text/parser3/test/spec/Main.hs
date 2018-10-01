module Main where

import Prelude
import Test.Hspec.Formatters
import Test.Hspec.Runner

import qualified Spec

main :: IO ()
main = hspecWith defaultConfig {configFormatter = Just progress} Spec.spec
