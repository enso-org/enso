module Data.Convert.Instances.Base where

import Data.Convert.Base

instance {-# OVERLAPPABLE #-}                  Castable [a] [a]  where cast = id        ; {-# INLINE cast #-}
instance {-# OVERLAPPABLE #-} Castable a a' => Castable [a] [a'] where cast = fmap cast ; {-# INLINE cast #-}
