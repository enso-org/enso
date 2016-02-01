module Data.Convert.Instances.Base where

import Data.Convert.Base

instance Castable a a' => Castable [a] [a'] where cast = fmap cast ; {-# INLINE cast #-}