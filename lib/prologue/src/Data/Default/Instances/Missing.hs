{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Default.Instances.Missing where

import Data.Text.Lazy           (Text)
import Data.Default



instance Default Text where def = ""
