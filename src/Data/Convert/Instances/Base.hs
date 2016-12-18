module Data.Convert.Instances.Base where

import Data.Convert.Class

instance {-# OVERLAPPABLE #-} Castable a a' => Castable [a] [a'] where cast = fmap cast ; {-# INLINE cast #-}
