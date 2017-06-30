module Data.Container.Mono.Instances.Set where

import Prologue
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Container.Mono.Class

instance Ord a => Empty (Set a)
instance Singleton (Set a) where singleton = Set.singleton
