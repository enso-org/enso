{-# LANGUAGE OverloadedStrings #-}

module Main where

import Luna.Prelude hiding (Level, switch, argument)
import qualified Luna.Shell as Shell
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map  as Map
import qualified Data.Text as Text
import qualified System.Environment as System
import qualified Text.PrettyPrint.ANSI.Leijen as Doc



-------------------
-- === Shell === --
-------------------

main :: IO ()
main = do
    Shell.main
