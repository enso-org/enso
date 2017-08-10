module Data.Convert.Instances.List where

import Prelude
import Data.Convert.Class
import Data.List.NonEmpty
import Data.Default

instance {-# OVERLAPPABLE #-} Convertible a [a] where convert = pure ; {-# INLINE convert #-}

instance PartialConvertible [a] (NonEmpty a) where
    type ConversionError    [a] (NonEmpty a) = SimpleConversionError
    unsafeConvert (e:es) = e :| es               ; {-# INLINE unsafeConvert #-}
    convertAssert        = defConvertAssert null ; {-# INLINE convertAssert #-}

instance Convertible' a b => Convertible (NonEmpty a) [b] where
    convert (a:|as) = convert' a : fmap convert' as
