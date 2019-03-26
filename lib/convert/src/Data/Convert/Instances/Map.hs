{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Convert.Instances.Map where

import Prelude
import Data.Convert.Class

import qualified Data.Map as Map

instance Ord k => Convertible [(k, a)]      (Map.Map k a) where convert = Map.fromList ; {-# INLINE convert #-}
instance          Convertible (Map.Map k a) [(k, a)]      where convert = Map.toList   ; {-# INLINE convert #-}

