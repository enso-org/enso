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
--   When trying to convert between the same types, compile time error is reported in order to help tracking not needed usages.
--   If you want to enable conversion between the same types, use `convert'` instead.
class Convertible  t t' where convert  ::                        t                -> t'
class Convertible1 t t' where convert1 :: forall s1.             t s1             -> t' s1
class Convertible2 t t' where convert2 :: forall s1 s2.          t s1 s2          -> t' s1 s2
class Convertible3 t t' where convert3 :: forall s1 s2 s3.       t s1 s2 s3       -> t' s1 s2 s3
class Convertible4 t t' where convert4 :: forall s1 s2 s3 s4.    t s1 s2 s3 s4    -> t' s1 s2 s3 s4
class Convertible5 t t' where convert5 :: forall s1 s2 s3 s4 s5. t s1 s2 s3 s4 s5 -> t' s1 s2 s3 s4 s5


-- === Identity conversion errors === --

type IdConversionErr (t :: k) = 'Text "Conversion of the same type (`" :<>: 'ShowType t :<>: 'Text "`) is disabled by default. Please use convert' if you want to enable it."
instance TypeError (IdConversionErr t) => Convertible  t t where convert  = id ; {-# INLINE convert  #-}
instance TypeError (IdConversionErr t) => Convertible1 t t where convert1 = id ; {-# INLINE convert1 #-}
instance TypeError (IdConversionErr t) => Convertible2 t t where convert2 = id ; {-# INLINE convert2 #-}
instance TypeError (IdConversionErr t) => Convertible3 t t where convert3 = id ; {-# INLINE convert3 #-}
instance TypeError (IdConversionErr t) => Convertible4 t t where convert4 = id ; {-# INLINE convert4 #-}
instance TypeError (IdConversionErr t) => Convertible5 t t where convert5 = id ; {-# INLINE convert5 #-}


-- === Utils === --

convertTo  :: forall t' t. Convertible  t t' =>                        t                -> t'
convert1To :: forall t' t. Convertible1 t t' => forall s1.             t s1             -> t' s1
convert2To :: forall t' t. Convertible2 t t' => forall s1 s2.          t s1 s2          -> t' s1 s2
convert3To :: forall t' t. Convertible3 t t' => forall s1 s2 s3.       t s1 s2 s3       -> t' s1 s2 s3
convert4To :: forall t' t. Convertible4 t t' => forall s1 s2 s3 s4.    t s1 s2 s3 s4    -> t' s1 s2 s3 s4
convert5To :: forall t' t. Convertible5 t t' => forall s1 s2 s3 s4 s5. t s1 s2 s3 s4 s5 -> t' s1 s2 s3 s4 s5
convertTo  = convert  ; {-# INLINE convertTo  #-}
convert1To = convert1 ; {-# INLINE convert1To #-}
convert2To = convert2 ; {-# INLINE convert2To #-}
convert3To = convert3 ; {-# INLINE convert3To #-}
convert4To = convert4 ; {-# INLINE convert4To #-}
convert5To = convert5 ; {-# INLINE convert5To #-}


-- === Conversion allowing the same types === --

class Convertible'  t t' where convert'  ::                        t                -> t'
class Convertible1' t t' where convert1' :: forall s1.             t s1             -> t' s1
class Convertible2' t t' where convert2' :: forall s1 s2.          t s1 s2          -> t' s1 s2
class Convertible3' t t' where convert3' :: forall s1 s2 s3.       t s1 s2 s3       -> t' s1 s2 s3
class Convertible4' t t' where convert4' :: forall s1 s2 s3 s4.    t s1 s2 s3 s4    -> t' s1 s2 s3 s4
class Convertible5' t t' where convert5' :: forall s1 s2 s3 s4 s5. t s1 s2 s3 s4 s5 -> t' s1 s2 s3 s4 s5

instance {-# OVERLAPPING #-} Convertible'  t t where convert'  = id ; {-# INLINE convert'  #-}
instance {-# OVERLAPPING #-} Convertible1' t t where convert1' = id ; {-# INLINE convert1' #-}
instance {-# OVERLAPPING #-} Convertible2' t t where convert2' = id ; {-# INLINE convert2' #-}
instance {-# OVERLAPPING #-} Convertible3' t t where convert3' = id ; {-# INLINE convert3' #-}
instance {-# OVERLAPPING #-} Convertible4' t t where convert4' = id ; {-# INLINE convert4' #-}
instance {-# OVERLAPPING #-} Convertible5' t t where convert5' = id ; {-# INLINE convert5' #-}
instance Convertible  t t' => Convertible'  t t' where convert'  = convert  ; {-# INLINE convert'  #-}
instance Convertible1 t t' => Convertible1' t t' where convert1' = convert1 ; {-# INLINE convert1' #-}
instance Convertible2 t t' => Convertible2' t t' where convert2' = convert2 ; {-# INLINE convert2' #-}
instance Convertible3 t t' => Convertible3' t t' where convert3' = convert3 ; {-# INLINE convert3' #-}
instance Convertible4 t t' => Convertible4' t t' where convert4' = convert4 ; {-# INLINE convert4' #-}
instance Convertible5 t t' => Convertible5' t t' where convert5' = convert5 ; {-# INLINE convert5' #-}

convertTo'  :: forall t' t. Convertible'  t t' =>                        t                -> t'
convert1To' :: forall t' t. Convertible1' t t' => forall s1.             t s1             -> t' s1
convert2To' :: forall t' t. Convertible2' t t' => forall s1 s2.          t s1 s2          -> t' s1 s2
convert3To' :: forall t' t. Convertible3' t t' => forall s1 s2 s3.       t s1 s2 s3       -> t' s1 s2 s3
convert4To' :: forall t' t. Convertible4' t t' => forall s1 s2 s3 s4.    t s1 s2 s3 s4    -> t' s1 s2 s3 s4
convert5To' :: forall t' t. Convertible5' t t' => forall s1 s2 s3 s4 s5. t s1 s2 s3 s4 s5 -> t' s1 s2 s3 s4 s5
convertTo'  = convert'  ; {-# INLINE convertTo'  #-}
convert1To' = convert1' ; {-# INLINE convert1To' #-}
convert2To' = convert2' ; {-# INLINE convert2To' #-}
convert3To' = convert3' ; {-# INLINE convert3To' #-}
convert4To' = convert4' ; {-# INLINE convert4To' #-}
convert5To' = convert5' ; {-# INLINE convert5To' #-}


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

-- FIXME: Depreciated
class Castable a b where
    cast :: a -> b
    default cast :: Convertible a b => a -> b
    cast = convert ; {-# INLINE cast #-}


-- === Isomorphisms === --

type IsoPartialConvertible  a b = (PartialConvertible a b, PartialConvertible b a)
type IsoConvertible         a b = (Convertible        a b, Convertible        b a)
type IsoConvertible'        a b = (Convertible'       a b, Convertible'       b a)
-- type IsoCastable            a b = (Castable           a b, Castable           b a)

converted  :: IsoConvertible  a b => Iso' a b
converted' :: IsoConvertible' a b => Iso' a b
-- casted     :: IsoCastable     a b => Iso' a b
converted  = iso convert  convert
converted' = iso convert' convert'
-- casted     = iso cast     cast

convertedTo  :: forall b a. IsoConvertible  a b => Iso' a b
convertedTo' :: forall b a. IsoConvertible' a b => Iso' a b
-- castedTo     :: forall b a. IsoCastable     a b => Iso' a b
convertedTo  = converted
convertedTo' = converted'
-- castedTo     = casted


-- === Basic instances === --

-- instance {-# OVERLAPPABLE #-} Castable a a where
--     cast = id

instance {-# OVERLAPPABLE #-} Convertible a b => Convertible (Maybe a) (Maybe b)where
    convert = fmap convert ; {-# INLINE convert #-}


-- === ConvertBy === --

type ConvertBy  p a b = (Convertible  a p, Convertible  p b)
type ConvertBy' p a b = (Convertible' a p, Convertible' p b)

convertVia :: forall p a b. ConvertBy p a b => a -> b
convertVia a = convert (convert a :: p) ; {-# INLINE convertVia #-}

convertVia' :: forall p a b. ConvertBy' p a b => a -> b
convertVia' a = convert' (convert' a :: p) ; {-# INLINE convertVia' #-}

--

-- class ConvertibleM  m n where convertM  :: m t1 -> n t1
-- class ConvertibleM2 m n where convertM2 :: m t1 t2 -> n t1 t2
-- class ConvertibleM3 m n where convertM3 :: m t1 t2 t3 -> n t1 t2 t3
-- class ConvertibleM4 m n where convertM4 :: m t1 t2 t3 t4 -> n t1 t2 t3 t4
-- class ConvertibleM5 m n where convertM5 :: m t1 t2 t3 t4 t5 -> n t1 t2 t3 t4 t5
