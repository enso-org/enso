module Data.Convert.Instances.Base where

import Data.Convert.Class

instance Convertible Char String where convert = pure
