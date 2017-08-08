module Data.Text32 (module Data.Text32, module X) where

import Prelude
import           Data.Container.Vector as X
import qualified Data.Vector.Unboxed   as Unboxed

type Text32 = Unboxed.Vector Char
