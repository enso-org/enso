module Main where

import Prelude

import qualified Luna.CI as CI

import qualified Spec

main :: IO ()
main = CI.main Spec.spec

