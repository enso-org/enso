module Data.Text32 (module Data.Text32, module X) where

import Prelude
import Data.Vector.Unboxed   as X -- (Vector, all, toList)

import Data.Container.Vector as X (index, commonPrefixes, replace)

type Text32 = Vector Char

