{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-patterns #-}

module Data.Convert.Instances.List where

import Prelude
import Data.Convert.Class
import Data.List.NonEmpty

instance {-# OVERLAPPABLE #-} Convertible a [a] where convert = pure ; {-# INLINE convert #-}

instance Convertible' a b => Convertible (NonEmpty a) [b] where
    convert (a:|as) = convert' a : fmap convert' as

instance Convertible' a b => PartialConvertible [a] (NonEmpty b) where
    type ConversionError [a] (NonEmpty b) = SimpleConversionError
    convertAssert = \case
        [] -> Just SimpleConversionError
        _  -> Nothing

    unsafeConvert (a:as) = convert' a :| fmap convert' as ; {-# INLINE unsafeConvert #-}

