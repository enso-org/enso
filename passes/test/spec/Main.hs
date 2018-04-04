module Main where

import Prelude

import Luna.CI

import qualified Spec

main :: IO ()
main = ciMain Spec.spec

