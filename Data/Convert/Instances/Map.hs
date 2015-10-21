module Data.Convert.Instances.Map()
where

import Data.Convert.Base

import qualified Data.Map as Map


instance Ord k => Convertible [(k, a)] (Map.Map k a) where
    convert = Map.fromList

instance Convertible (Map.Map k a) [(k, a)] where
    convert = Map.toList
