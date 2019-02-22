module Prologue.Data.Ix (module Prologue.Data.Ix, module X) where

import Prelude
import           Data.Ix as X (Ix, range, inRange, rangeSize)
import qualified Data.Ix as Ix

rangeIndex :: Ix a => (a, a) -> a -> Int
rangeIndex = Ix.index ; {-# INLINE rangeIndex #-}
