module Data.Convert.Instances.ByteString where

import qualified Data.ByteString      as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.UTF8 as UTF8
import Data.Convert.Class

instance Convertible Strict.ByteString String            where convert = UTF8.toString
instance Convertible String            Strict.ByteString where convert = UTF8.fromString

instance Convertible Lazy.ByteString String          where convert = convertVia @Strict.ByteString
instance Convertible String          Lazy.ByteString where convert = convertVia @Strict.ByteString

instance Convertible Lazy.ByteString   Strict.ByteString where convert = Lazy.toStrict
instance Convertible Strict.ByteString Lazy.ByteString   where convert = Lazy.fromStrict
