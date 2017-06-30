module Data.Container.Map.IntMap where

import Prologue
import qualified Data.IntMap.Lazy as IM
import GHC.Exts (IsList)

newtype IntMap k v = IntMap (IM.IntMap v) deriving (Functor, Foldable, Traversable, Eq, Data, Ord, Read, Show, Semigroup, Mempty, NFData)
