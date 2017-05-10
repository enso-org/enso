module Data.Convert.Instances.ByteString where

import qualified Data.ByteString      as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.UTF8 as UTF8
import Data.Convert.Class

instance Convertible Strict.ByteString String            where convert = UTF8.toString
instance Convertible String            Strict.ByteString where convert = UTF8.fromString
