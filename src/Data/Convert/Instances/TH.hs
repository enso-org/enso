{-# LANGUAGE TemplateHaskell #-}

module Data.Convert.Instances.TH where

import Data.Convert.Class
import Data.Convert.Bound
import Data.Convert.Instances.Num (numConversions)


genConversions numConversions
