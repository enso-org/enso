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
import Data.Default

-- TODO[WD]: file GHC Bug:
-- data A a = A a
-- data B a = B a deriving (Functor)
-- data C = C
--
-- instance Convertible1 A B
-- instance Convertible (A a) (B a)
--
-- test :: A a -> B a' -- WRONG CONSTRAINT INFERRED HERE! FILE A BUG
-- test = convert'


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

-- | We cannot use OVERLAPPABLE instances here. Let's consider following instance:
--       instance (s ~ s', t ~ t') => Convertible (Symbol s t) (Symbol2 s' t') where ...
--   By writing it we indicate that during conversion from Symbol to Symbol2 the appropriate variables should be matched.
--   If the following instances would be declared as OVERLAPPABLE, it will clash with the instance `Convertible (t a) (t' a)`.
--   In other words, if user defines any conversion between type `X a` and `Y b`, we disable automatic lifting to `ConvertibleN`.
--   TODO[WD]: File GHC proposal to choose more specific incoherent instance
instance {-# OVERLAPPABLE #-}                  (Convertible  a a', Functor t)  => Convertible (t (a :: *)) (t  a') where convert = fmap convert             ; {-# INLINE convert #-}
instance {-# INCOHERENT #-} (Convertible1 t t', Convertible' a a', Functor t') => Convertible (t (a :: *)) (t' a') where convert = fmap convert' . convert1 ; {-# INLINE convert #-}
instance {-# INCOHERENT #-} (Convertible1 t t')                                => Convertible (t (a :: *)) (t' a ) where convert = convert1                 ; {-# INLINE convert #-}

instance {-# OVERLAPPABLE #-} Convertible2 t t' => Convertible1 (t a) (t' a) where convert1 = convert2 ; {-# INLINE convert1 #-}
instance {-# OVERLAPPABLE #-} Convertible3 t t' => Convertible2 (t a) (t' a) where convert2 = convert3 ; {-# INLINE convert2 #-}
instance {-# OVERLAPPABLE #-} Convertible4 t t' => Convertible3 (t a) (t' a) where convert3 = convert4 ; {-# INLINE convert3 #-}
instance {-# OVERLAPPABLE #-} Convertible5 t t' => Convertible4 (t a) (t' a) where convert4 = convert5 ; {-# INLINE convert4 #-}


-- === Identity conversion errors === --

type IdConversionErr (t :: k) = 'Text "Conversion of the same type (`"
                          ':<>: 'ShowType t
                          ':<>: 'Text "`) is disabled by default. Please use convert' if you want to enable it."
instance TypeError (IdConversionErr t) => Convertible  t t where convert  = id ; {-# INLINE convert  #-}
instance TypeError (IdConversionErr t) => Convertible1 t t where convert1 = id ; {-# INLINE convert1 #-}
instance TypeError (IdConversionErr t) => Convertible2 t t where convert2 = id ; {-# INLINE convert2 #-}
instance TypeError (IdConversionErr t) => Convertible3 t t where convert3 = id ; {-# INLINE convert3 #-}
instance TypeError (IdConversionErr t) => Convertible4 t t where convert4 = id ; {-# INLINE convert4 #-}
instance TypeError (IdConversionErr t) => Convertible5 t t where convert5 = id ; {-# INLINE convert5 #-}


-- === Utils === --

convertTo  :: forall t' t. Convertible  t t' =>                        t                -> t'
convertTo1 :: forall t' t. Convertible1 t t' => forall s1.             t s1             -> t' s1
convertTo2 :: forall t' t. Convertible2 t t' => forall s1 s2.          t s1 s2          -> t' s1 s2
convertTo3 :: forall t' t. Convertible3 t t' => forall s1 s2 s3.       t s1 s2 s3       -> t' s1 s2 s3
convertTo4 :: forall t' t. Convertible4 t t' => forall s1 s2 s3 s4.    t s1 s2 s3 s4    -> t' s1 s2 s3 s4
convertTo5 :: forall t' t. Convertible5 t t' => forall s1 s2 s3 s4 s5. t s1 s2 s3 s4 s5 -> t' s1 s2 s3 s4 s5
convertTo  = convert  ; {-# INLINE convertTo  #-}
convertTo1 = convert1 ; {-# INLINE convertTo1 #-}
convertTo2 = convert2 ; {-# INLINE convertTo2 #-}
convertTo3 = convert3 ; {-# INLINE convertTo3 #-}
convertTo4 = convert4 ; {-# INLINE convertTo4 #-}
convertTo5 = convert5 ; {-# INLINE convertTo5 #-}


-- === Conversion allowing the same types === --

class Convertible'  t t' where convert'  ::                        t                -> t'
class Convertible1' t t' where convert1' :: forall s1.             t s1             -> t' s1
class Convertible2' t t' where convert2' :: forall s1 s2.          t s1 s2          -> t' s1 s2
class Convertible3' t t' where convert3' :: forall s1 s2 s3.       t s1 s2 s3       -> t' s1 s2 s3
class Convertible4' t t' where convert4' :: forall s1 s2 s3 s4.    t s1 s2 s3 s4    -> t' s1 s2 s3 s4
class Convertible5' t t' where convert5' :: forall s1 s2 s3 s4 s5. t s1 s2 s3 s4 s5 -> t' s1 s2 s3 s4 s5

instance {-# OVERLAPPABLE #-}                      Convertible'  t t  where convert'  = id       ; {-# INLINE convert'  #-}
instance {-# OVERLAPPABLE #-}                      Convertible1' t t  where convert1' = id       ; {-# INLINE convert1' #-}
instance {-# OVERLAPPABLE #-}                      Convertible2' t t  where convert2' = id       ; {-# INLINE convert2' #-}
instance {-# OVERLAPPABLE #-}                      Convertible3' t t  where convert3' = id       ; {-# INLINE convert3' #-}
instance {-# OVERLAPPABLE #-}                      Convertible4' t t  where convert4' = id       ; {-# INLINE convert4' #-}
instance {-# OVERLAPPABLE #-}                      Convertible5' t t  where convert5' = id       ; {-# INLINE convert5' #-}
instance {-# OVERLAPPABLE #-} Convertible  t t' => Convertible'  t t' where convert'  = convert  ; {-# INLINE convert'  #-}
instance {-# OVERLAPPABLE #-} Convertible1 t t' => Convertible1' t t' where convert1' = convert1 ; {-# INLINE convert1' #-}
instance {-# OVERLAPPABLE #-} Convertible2 t t' => Convertible2' t t' where convert2' = convert2 ; {-# INLINE convert2' #-}
instance {-# OVERLAPPABLE #-} Convertible3 t t' => Convertible3' t t' where convert3' = convert3 ; {-# INLINE convert3' #-}
instance {-# OVERLAPPABLE #-} Convertible4 t t' => Convertible4' t t' where convert4' = convert4 ; {-# INLINE convert4' #-}
instance {-# OVERLAPPABLE #-} Convertible5 t t' => Convertible5' t t' where convert5' = convert5 ; {-# INLINE convert5' #-}

convertTo'  :: forall t' t. Convertible'  t t' =>                        t                -> t'
convertTo1' :: forall t' t. Convertible1' t t' => forall s1.             t s1             -> t' s1
convertTo2' :: forall t' t. Convertible2' t t' => forall s1 s2.          t s1 s2          -> t' s1 s2
convertTo3' :: forall t' t. Convertible3' t t' => forall s1 s2 s3.       t s1 s2 s3       -> t' s1 s2 s3
convertTo4' :: forall t' t. Convertible4' t t' => forall s1 s2 s3 s4.    t s1 s2 s3 s4    -> t' s1 s2 s3 s4
convertTo5' :: forall t' t. Convertible5' t t' => forall s1 s2 s3 s4 s5. t s1 s2 s3 s4 s5 -> t' s1 s2 s3 s4 s5
convertTo'  = convert'  ; {-# INLINE convertTo'  #-}
convertTo1' = convert1' ; {-# INLINE convertTo1' #-}
convertTo2' = convert2' ; {-# INLINE convertTo2' #-}
convertTo3' = convert3' ; {-# INLINE convertTo3' #-}
convertTo4' = convert4' ; {-# INLINE convertTo4' #-}
convertTo5' = convert5' ; {-# INLINE convertTo5' #-}



----------------------------------
-- === Partial convertibles === --
----------------------------------

-- === Errors === --

data SimpleConversionError = SimpleConversionError deriving (Show)
instance Default SimpleConversionError where def = SimpleConversionError ; {-# INLINE def #-}


-- === Classes === --

-- | PartialConvertible allows conversions that could fail with `ConversionError`.
class PartialConvertible t t' where
    type family ConversionError t t'
    convertAssert :: t -> Maybe (ConversionError t t')
    unsafeConvert :: t -> t'

defConvertAssert :: Default e => (a -> Bool) -> a -> Maybe e
defConvertAssert f = \s -> if f s then Just def else Nothing

unsafeConvertTo :: forall t' t. PartialConvertible t t' => t -> t'
unsafeConvertTo = unsafeConvert ; {-# INLINE unsafeConvertTo #-}

convertAssertTo :: forall t' t. PartialConvertible t t' => t -> Maybe (ConversionError t t')
convertAssertTo = convertAssert @t @t' ; {-# INLINE convertAssertTo #-}

maybeConvert :: forall t t'. PartialConvertible t t' => t -> Maybe t'
maybeConvert t = const (unsafeConvert t) <$> convertAssertTo @t' t ; {-# INLINE maybeConvert #-}

tryConvert :: forall t t'. PartialConvertible t t' => t -> Either (ConversionError t t') t'
tryConvert t = maybe (Right $ unsafeConvert t) Left $ convertAssertTo @t' t ; {-# INLINE tryConvert #-}



-----------------------------
-- === Bi-convertibles === --
-----------------------------

type BiConvertible  t t' = (Convertible  t t', Convertible  t' t)
type BiConvertible1 t t' = (Convertible1 t t', Convertible1 t' t)
type BiConvertible2 t t' = (Convertible2 t t', Convertible2 t' t)
type BiConvertible3 t t' = (Convertible3 t t', Convertible3 t' t)
type BiConvertible4 t t' = (Convertible4 t t', Convertible4 t' t)
type BiConvertible5 t t' = (Convertible5 t t', Convertible5 t' t)

type BiConvertible'  t t' = (Convertible'  t t', Convertible'  t' t)
type BiConvertible1' t t' = (Convertible1' t t', Convertible1' t' t)
type BiConvertible2' t t' = (Convertible2' t t', Convertible2' t' t)
type BiConvertible3' t t' = (Convertible3' t t', Convertible3' t' t)
type BiConvertible4' t t' = (Convertible4' t t', Convertible4' t' t)
type BiConvertible5' t t' = (Convertible5' t t', Convertible5' t' t)

type BiPartialConvertible t t' = (PartialConvertible t t', PartialConvertible t t')

converted   :: BiConvertible   t t' =>                        Iso' t                  t'
converted1  :: BiConvertible1  t t' => forall s1.             Iso' (t s1)             (t' s1)
converted2  :: BiConvertible2  t t' => forall s1 s2.          Iso' (t s1 s2)          (t' s1 s2)
converted3  :: BiConvertible3  t t' => forall s1 s2 s3.       Iso' (t s1 s2 s3)       (t' s1 s2 s3)
converted4  :: BiConvertible4  t t' => forall s1 s2 s3 s4.    Iso' (t s1 s2 s3 s4)    (t' s1 s2 s3 s4)
converted5  :: BiConvertible5  t t' => forall s1 s2 s3 s4 s5. Iso' (t s1 s2 s3 s4 s5) (t' s1 s2 s3 s4 s5)
converted'  :: BiConvertible'  t t' =>                        Iso' t                  t'
converted1' :: BiConvertible1' t t' => forall s1.             Iso' (t s1)             (t' s1)
converted2' :: BiConvertible2' t t' => forall s1 s2.          Iso' (t s1 s2)          (t' s1 s2)
converted3' :: BiConvertible3' t t' => forall s1 s2 s3.       Iso' (t s1 s2 s3)       (t' s1 s2 s3)
converted4' :: BiConvertible4' t t' => forall s1 s2 s3 s4.    Iso' (t s1 s2 s3 s4)    (t' s1 s2 s3 s4)
converted5' :: BiConvertible5' t t' => forall s1 s2 s3 s4 s5. Iso' (t s1 s2 s3 s4 s5) (t' s1 s2 s3 s4 s5)
converted   = iso convert   convert   ; {-# INLINE converted   #-}
converted1  = iso convert1  convert1  ; {-# INLINE converted1  #-}
converted2  = iso convert2  convert2  ; {-# INLINE converted2  #-}
converted3  = iso convert3  convert3  ; {-# INLINE converted3  #-}
converted4  = iso convert4  convert4  ; {-# INLINE converted4  #-}
converted5  = iso convert5  convert5  ; {-# INLINE converted5  #-}
converted'  = iso convert'  convert'  ; {-# INLINE converted'  #-}
converted1' = iso convert1' convert1' ; {-# INLINE converted1' #-}
converted2' = iso convert2' convert2' ; {-# INLINE converted2' #-}
converted3' = iso convert3' convert3' ; {-# INLINE converted3' #-}
converted4' = iso convert4' convert4' ; {-# INLINE converted4' #-}
converted5' = iso convert5' convert5' ; {-# INLINE converted5' #-}

convertedTo   :: BiConvertible   t' t =>                        Iso' t                  t'
convertedTo1  :: BiConvertible1  t' t => forall s1.             Iso' (t s1)             (t' s1)
convertedTo2  :: BiConvertible2  t' t => forall s1 s2.          Iso' (t s1 s2)          (t' s1 s2)
convertedTo3  :: BiConvertible3  t' t => forall s1 s2 s3.       Iso' (t s1 s2 s3)       (t' s1 s2 s3)
convertedTo4  :: BiConvertible4  t' t => forall s1 s2 s3 s4.    Iso' (t s1 s2 s3 s4)    (t' s1 s2 s3 s4)
convertedTo5  :: BiConvertible5  t' t => forall s1 s2 s3 s4 s5. Iso' (t s1 s2 s3 s4 s5) (t' s1 s2 s3 s4 s5)
convertedTo'  :: BiConvertible'  t' t =>                        Iso' t                  t'
convertedTo1' :: BiConvertible1' t' t => forall s1.             Iso' (t s1)             (t' s1)
convertedTo2' :: BiConvertible2' t' t => forall s1 s2.          Iso' (t s1 s2)          (t' s1 s2)
convertedTo3' :: BiConvertible3' t' t => forall s1 s2 s3.       Iso' (t s1 s2 s3)       (t' s1 s2 s3)
convertedTo4' :: BiConvertible4' t' t => forall s1 s2 s3 s4.    Iso' (t s1 s2 s3 s4)    (t' s1 s2 s3 s4)
convertedTo5' :: BiConvertible5' t' t => forall s1 s2 s3 s4 s5. Iso' (t s1 s2 s3 s4 s5) (t' s1 s2 s3 s4 s5)
convertedTo   = converted   ; {-# INLINE convertedTo   #-}
convertedTo1  = converted1  ; {-# INLINE convertedTo1  #-}
convertedTo2  = converted2  ; {-# INLINE convertedTo2  #-}
convertedTo3  = converted3  ; {-# INLINE convertedTo3  #-}
convertedTo4  = converted4  ; {-# INLINE convertedTo4  #-}
convertedTo5  = converted5  ; {-# INLINE convertedTo5  #-}
convertedTo'  = converted'  ; {-# INLINE convertedTo'  #-}
convertedTo1' = converted1' ; {-# INLINE convertedTo1' #-}
convertedTo2' = converted2' ; {-# INLINE convertedTo2' #-}
convertedTo3' = converted3' ; {-# INLINE convertedTo3' #-}
convertedTo4' = converted4' ; {-# INLINE convertedTo4' #-}
convertedTo5' = converted5' ; {-# INLINE convertedTo5' #-}


-- === ConvertibleVia === --

type ConvertibleVia  t p t' = (Convertible  t p, Convertible  p t')
type ConvertibleVia1 t p t' = (Convertible1 t p, Convertible1 p t')
type ConvertibleVia2 t p t' = (Convertible2 t p, Convertible2 p t')
type ConvertibleVia3 t p t' = (Convertible3 t p, Convertible3 p t')
type ConvertibleVia4 t p t' = (Convertible4 t p, Convertible4 p t')
type ConvertibleVia5 t p t' = (Convertible5 t p, Convertible5 p t')

convertVia  :: forall p t t'. ConvertibleVia  t p t' =>                        t                -> t'
convertVia1 :: forall p t t'. ConvertibleVia1 t p t' => forall s1.             t s1             -> t' s1
convertVia2 :: forall p t t'. ConvertibleVia2 t p t' => forall s1 s2.          t s1 s2          -> t' s1 s2
convertVia3 :: forall p t t'. ConvertibleVia3 t p t' => forall s1 s2 s3.       t s1 s2 s3       -> t' s1 s2 s3
convertVia4 :: forall p t t'. ConvertibleVia4 t p t' => forall s1 s2 s3 s4.    t s1 s2 s3 s4    -> t' s1 s2 s3 s4
convertVia5 :: forall p t t'. ConvertibleVia5 t p t' => forall s1 s2 s3 s4 s5. t s1 s2 s3 s4 s5 -> t' s1 s2 s3 s4 s5
convertVia  = convert  . convertTo  @p ; {-# INLINE convertVia #-}
convertVia1 = convert1 . convertTo1 @p ; {-# INLINE convertVia1 #-}
convertVia2 = convert2 . convertTo2 @p ; {-# INLINE convertVia2 #-}
convertVia3 = convert3 . convertTo3 @p ; {-# INLINE convertVia3 #-}
convertVia4 = convert4 . convertTo4 @p ; {-# INLINE convertVia4 #-}
convertVia5 = convert5 . convertTo5 @p ; {-# INLINE convertVia5 #-}

type ConvertibleVia'  t p t' = (Convertible'  t p, Convertible'  p t')
type ConvertibleVia1' t p t' = (Convertible1' t p, Convertible1' p t')
type ConvertibleVia2' t p t' = (Convertible2' t p, Convertible2' p t')
type ConvertibleVia3' t p t' = (Convertible3' t p, Convertible3' p t')
type ConvertibleVia4' t p t' = (Convertible4' t p, Convertible4' p t')
type ConvertibleVia5' t p t' = (Convertible5' t p, Convertible5' p t')

convertVia'  :: forall p t t'. ConvertibleVia'  t p t' =>                        t                -> t'
convertVia1' :: forall p t t'. ConvertibleVia1' t p t' => forall s1.             t s1             -> t' s1
convertVia2' :: forall p t t'. ConvertibleVia2' t p t' => forall s1 s2.          t s1 s2          -> t' s1 s2
convertVia3' :: forall p t t'. ConvertibleVia3' t p t' => forall s1 s2 s3.       t s1 s2 s3       -> t' s1 s2 s3
convertVia4' :: forall p t t'. ConvertibleVia4' t p t' => forall s1 s2 s3 s4.    t s1 s2 s3 s4    -> t' s1 s2 s3 s4
convertVia5' :: forall p t t'. ConvertibleVia5' t p t' => forall s1 s2 s3 s4 s5. t s1 s2 s3 s4 s5 -> t' s1 s2 s3 s4 s5
convertVia'  = convert'  . convertTo'  @p ; {-# INLINE convertVia' #-}
convertVia1' = convert1' . convertTo1' @p ; {-# INLINE convertVia1' #-}
convertVia2' = convert2' . convertTo2' @p ; {-# INLINE convertVia2' #-}
convertVia3' = convert3' . convertTo3' @p ; {-# INLINE convertVia3' #-}
convertVia4' = convert4' . convertTo4' @p ; {-# INLINE convertVia4' #-}
convertVia5' = convert5' . convertTo5' @p ; {-# INLINE convertVia5' #-}

type PartialConvertibleVia  t p t' = (PartialConvertible  t p, PartialConvertible  p t')
unsafeConvertVia :: forall p t t'. PartialConvertibleVia t p t' => t -> t'
unsafeConvertVia = unsafeConvert . unsafeConvertTo @p ; {-# INLINE unsafeConvertVia #-}