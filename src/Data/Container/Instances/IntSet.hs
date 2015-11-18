module Data.Container.Instances.IntSet where

import Prelude

import           Data.Container.Class
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Container.List

type instance Item IntSet = Int

instance ToList IntSet where toList = IntSet.toList