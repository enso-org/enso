{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE TemplateHaskell #-}

module Data.Convert.Instances.TH where

import Data.Convert.Class
import Data.Convert.Instances.Num as Num
import Data.Proxy

Num.conversions

