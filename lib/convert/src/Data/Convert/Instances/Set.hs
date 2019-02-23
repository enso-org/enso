{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Convert.Instances.Set where

import Prelude

import Data.Convert.Class

import qualified Data.Set as Set
import           Data.Set (Set)

instance          Convertible (Set a) [a]     where convert = Set.toList   ; {-# INLINE convert #-}
instance Ord a => Convertible [a]     (Set a) where convert = Set.fromList ; {-# INLINE convert #-}

