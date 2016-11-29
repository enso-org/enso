{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Convert.Instances () where

import Data.Convert.Base
import Data.Convert.Bound


genConversions numConversions
