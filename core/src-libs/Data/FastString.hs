{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# EXT      InlineAll            #-}

module Data.FastString (module X) where

import Luna.Prelude
import FastString   as X
import Data.Binary  (put, get)


-- === FastString Instances === --

-- Normal Form
instance NFData FastString where rnf = const ()

-- Read
instance Read FastString where readPrec = fromString <$> readPrec

-- Monoid
instance Mempty    FastString where mempty = ""
instance Semigroup FastString where a <> b = concatFS [a,b]

-- Construction
instance IsString FastString where fromString = convert
instance ToString FastString where toString   = convert

-- Repr
-- instance Repr s String => Repr s FastString where repr = repr ∘ toString

-- Conversions
instance Convertible FastString String     where convert = unpackFS
instance Convertible String     FastString where convert = fsLit

-- Binary
instance Binary FastString where
    put = put . convertTo @String
    get = convert @String <$> get
