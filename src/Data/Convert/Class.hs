{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell     #-}

module Data.Convert.Class where

import Prelude
import Control.Lens
import GHC.TypeLits



--------------------
-- === Errors === --
--------------------


newtype SimpleConversionError = SimpleConversionError String
makeWrapped ''SimpleConversionError

instance Show SimpleConversionError where show = view _Wrapped

simpleConversionError = SimpleConversionError "ConversionError"



--------------------------
-- === Convertibles === --
--------------------------

-- === Distinct types conversion === --

-- | Convertible allows for conversion between two compatible types.
--   It cannot be used to convert between the same type in order to track not needed convet usages.
--   If you want to enable conversion between the same type (which is just `id` function), use `convert'` instead.
class Convertible a b where
    convert :: a -> b

instance TypeError ( 'Text "Conversion of the same type (`"
                :<>: 'ShowType a
                :<>: 'Text "`) is disabled by default. Please use convert' if you want to enable it.")
      => Convertible a a where convert = id ; {-# INLINE convert #-}

convertTo :: forall b a. Convertible a b => a -> b
convertTo = convert


-- === Conversion allowing the same types === --

class                        Convertible' a b where convert' :: a -> b
instance {-# OVERLAPPING #-} Convertible' a a where convert' = id      ; {-# INLINE convert' #-}
instance Convertible a b =>  Convertible' a b where convert' = convert ; {-# INLINE convert' #-}

convertTo' :: forall b a. Convertible' a b => a -> b
convertTo' = convert'


-- === Partial conversions === --

-- | PartialConvertible allows conversions that could fail with `ConversionError`.
type family ConversionError a b
class PartialConvertible a b where
    tryConvert :: a -> Either (ConversionError a b) b
    default tryConvert :: Convertible a b => a -> Either (ConversionError a b) b
    tryConvert = Right . convert ; {-# INLINE tryConvert #-}

maybeConvert :: PartialConvertible a b => a -> Maybe b
maybeConvert a = case tryConvert a of
    Left  _ -> Nothing
    Right a -> Just a
{-# INLINE maybeConvert #-}

unsafeConvert :: Show (ConversionError a b) => PartialConvertible a b => a -> b
unsafeConvert a = case tryConvert a of
    Left  e -> error $ show e
    Right r -> r
{-# INLINE unsafeConvert #-}


-- === Casts === --

class Castable a b where
    cast :: a -> b
    default cast :: Convertible a b => a -> b
    cast = convert ; {-# INLINE cast #-}


-- === Isomorphisms === --

type IsoPartialConvertible  a b = (PartialConvertible a b, PartialConvertible b a)
type IsoConvertible         a b = (Convertible        a b, Convertible        b a)
type IsoConvertible'        a b = (Convertible'       a b, Convertible'       b a)
type IsoCastable            a b = (Castable           a b, Castable           b a)

converted  :: IsoConvertible  a b => Iso' a b
converted' :: IsoConvertible' a b => Iso' a b
casted     :: IsoCastable     a b => Iso' a b
converted  = iso convert  convert  ; {-# INLINE converted  #-}
converted' = iso convert' convert' ; {-# INLINE converted' #-}
casted     = iso cast     cast     ; {-# INLINE casted     #-}

convertedTo  :: forall b a. IsoConvertible  a b => Iso' a b
convertedTo' :: forall b a. IsoConvertible' a b => Iso' a b
castedTo     :: forall b a. IsoCastable     a b => Iso' a b
convertedTo  = converted
convertedTo' = converted'
castedTo     = casted


-- === Basic instances === --

instance {-# OVERLAPPABLE #-} Castable a a where
    cast = id ; {-# INLINE cast #-}

instance {-# OVERLAPPABLE #-} Convertible a b => Convertible (Maybe a) (Maybe b)where
    convert = fmap convert ; {-# INLINE convert #-}


-- === ConvertBy === --

type ConvertBy p a b = (Convertible a p, Convertible p b)

convertVia :: forall p a b. ConvertBy p a b => a -> b
convertVia a = convert (convert a :: p)

--

-- class ConvertibleM  m n where convertM  :: m t1 -> n t1
-- class ConvertibleM2 m n where convertM2 :: m t1 t2 -> n t1 t2
-- class ConvertibleM3 m n where convertM3 :: m t1 t2 t3 -> n t1 t2 t3
-- class ConvertibleM4 m n where convertM4 :: m t1 t2 t3 t4 -> n t1 t2 t3 t4
-- class ConvertibleM5 m n where convertM5 :: m t1 t2 t3 t4 t5 -> n t1 t2 t3 t4 t5
