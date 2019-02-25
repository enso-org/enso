{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell     #-}

module Data.Convert2.Class where

import Prelude

import qualified Data.Convert2.TH as TH

-- import Control.Lens
import GHC.TypeLits
import Data.Impossible (impossible)

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


type IdConversionErr (a :: k)
    = 'Text "Conversion of the same type (`" ':<>: 'ShowType a ':<>: 'Text "`)"
 ':<>: 'Text " is disabled by default. Please use convert' if you want to enable it."


------------------------
-- === Convertibe === --
------------------------

-- === Definition === --

-- > class To<N> tgt src where
-- >     to<N> :: ∀ t1 t2 ... t<N>. src t1 t2 ... t<N> -> tgt
-- > class From<N> tgt src where
-- >     from<N> :: ∀ t1 t2 .. t<N>. tgt -> src t1 t2 ... t<N>
TH.genConvertibleClasses False (TH.ixedName  "To")   (TH.ixedName  "to")
TH.genConvertibleClasses False (TH.ixedName' "To")   (TH.ixedName' "to")
TH.genConvertibleClasses True  (TH.ixedName  "From") (TH.ixedName  "from")
TH.genConvertibleClasses True  (TH.ixedName' "From") (TH.ixedName' "from")

-- > class UnsafeTo<N> tgt src where
-- >     unsafeTo<N> :: ∀ t1 t2 ... t<N>. src t1 t2 ... t<N> -> tgt
-- > class UnsafeFrom<N> tgt src where
-- >     unsafeFrom<N> :: ∀ t1 t2 .. t<N>. tgt -> src t1 t2 ... t<N>
TH.genConvertibleClasses False (TH.ixedName  "UnsafeTo")   (TH.ixedName  "unsafeTo")
TH.genConvertibleClasses False (TH.ixedName' "UnsafeTo")   (TH.ixedName' "unsafeTo")
TH.genConvertibleClasses True  (TH.ixedName  "UnsafeFrom") (TH.ixedName  "unsafeFrom")
TH.genConvertibleClasses True  (TH.ixedName' "UnsafeFrom") (TH.ixedName' "unsafeFrom")


-- === API Aliases === --

convert :: ∀ tgt src. To tgt src => src -> tgt
convert = to ; {-# INLINE convert #-}

convert1 :: ∀ tgt src t1. To1 tgt src => src t1 -> tgt
convert1 = to1 ; {-# INLINE convert1 #-}

convert2 :: ∀ tgt src t1 t2. To2 tgt src => src t1 t2 -> tgt
convert2 = to2 ; {-# INLINE convert2 #-}

unsafeConvert :: ∀ tgt src. UnsafeTo tgt src => src -> tgt
unsafeConvert = unsafeTo ; {-# INLINE unsafeConvert #-}

unsafeConvert1 :: ∀ tgt src t1. UnsafeTo1 tgt src => src t1 -> tgt
unsafeConvert1 = unsafeTo1 ; {-# INLINE unsafeConvert1 #-}

unsafeConvert2 :: ∀ tgt src t1 t2. UnsafeTo2 tgt src => src t1 t2 -> tgt
unsafeConvert2 = unsafeTo2 ; {-# INLINE unsafeConvert2 #-}


convert' :: ∀ tgt src. To' tgt src => src -> tgt
convert' = to' ; {-# INLINE convert' #-}

convert1' :: ∀ tgt src t1. To1' tgt src => src t1 -> tgt
convert1' = to1' ; {-# INLINE convert1' #-}

convert2' :: ∀ tgt src t1 t2. To2' tgt src => src t1 t2 -> tgt
convert2' = to2' ; {-# INLINE convert2' #-}

unsafeConvert' :: ∀ tgt src. UnsafeTo' tgt src => src -> tgt
unsafeConvert' = unsafeTo' ; {-# INLINE unsafeConvert' #-}

unsafeConvert1' :: ∀ tgt src t1. UnsafeTo1' tgt src => src t1 -> tgt
unsafeConvert1' = unsafeTo1' ; {-# INLINE unsafeConvert1' #-}

unsafeConvert2' :: ∀ tgt src t1 t2. UnsafeTo2' tgt src => src t1 t2 -> tgt
unsafeConvert2' = unsafeTo2' ; {-# INLINE unsafeConvert2' #-}


-- === Preventing id-conversions === --

-- > instance TypeError (IdConversionErr src)
-- >       => To<N> (src t1 t2 ... t<N>) src where
-- >     to<N> = impossible
-- >     {-# INLINE to<N> #-}
--
-- > instance TypeError (IdConversionErr src)
-- >       => From<N> (src t1 t2 ... t<N>) src where
-- >     from<N> = impossible
-- >     {-# INLINE from<N> #-}
TH.genIdConversionErrorInstances (TH.ixedName "To")         (TH.ixedName "to")
TH.genIdConversionErrorInstances (TH.ixedName "From")       (TH.ixedName "from")
TH.genIdConversionErrorInstances (TH.ixedName "UnsafeTo")   (TH.ixedName "unsafeTo")
TH.genIdConversionErrorInstances (TH.ixedName "UnsafeFrom") (TH.ixedName "unsafeFrom")


-- === Higher kind defaulting === --

-- > instance {-# OVERLAPPABLE #-} To<N+1> a a'
-- >       => To<N> (a t) a' where
-- >    to<N> = to<N+1>
-- >    {-# INLINE to<N> #-}
--
-- > instance {-# OVERLAPPABLE #-} From<N+1> a a'
-- >       => From<N> (a t) a' where
-- >    from<N> = from<N+1>
-- >    {-# INLINE from<N> #-}
TH.genHigherKindDefInstances (TH.ixedName "To")         (TH.ixedName "to")
TH.genHigherKindDefInstances (TH.ixedName "From")       (TH.ixedName "from")
TH.genHigherKindDefInstances (TH.ixedName "UnsafeTo")   (TH.ixedName "unsafeTo")
TH.genHigherKindDefInstances (TH.ixedName "UnsafeFrom") (TH.ixedName "unsafeFrom")



-- === Bi-convertibles === --

type Bi  tgt src = (To  tgt src, From  tgt src)
type Bi1 tgt src = (To1 tgt src, From1 tgt src)
type Bi2 tgt src = (To2 tgt src, From2 tgt src)
type Bi3 tgt src = (To3 tgt src, From3 tgt src)
type Bi4 tgt src = (To4 tgt src, From4 tgt src)
type Bi5 tgt src = (To5 tgt src, From5 tgt src)

type Bi'  tgt src = (To'  tgt src, From'  tgt src)
type Bi1' tgt src = (To1' tgt src, From1' tgt src)
type Bi2' tgt src = (To2' tgt src, From2' tgt src)
type Bi3' tgt src = (To3' tgt src, From3' tgt src)
type Bi4' tgt src = (To4' tgt src, From4' tgt src)
type Bi5' tgt src = (To5' tgt src, From5' tgt src)


instance {-# OVERLAPPABLE #-} To' a a where to' = id ; {-# INLINE to' #-}
instance To a b => To' a b where to' = to ; {-# INLINE to' #-}

instance {-# OVERLAPPABLE #-} From' a a where from' = id ; {-# INLINE from' #-}
instance From a b => From' a b where from' = from ; {-# INLINE from' #-}


-- -- === Utils === --

-- convertTo  :: ∀ a' a. Convertible  a a' =>              a           -> a'
-- convertTo1 :: ∀ a' a. Convertible1 a a' => ∀ b.         a b         -> a'
-- convertTo2 :: ∀ a' a. Convertible2 a a' => ∀ b c.       a b c       -> a'
-- convertTo3 :: ∀ a' a. Convertible3 a a' => ∀ b c d.     a b c d     -> a'
-- convertTo4 :: ∀ a' a. Convertible4 a a' => ∀ b c d e.   a b c d e   -> a'
-- convertTo5 :: ∀ a' a. Convertible5 a a' => ∀ b c d e f. a b c d e f -> a'
-- convertTo  = convert  ; {-# INLINE convertTo  #-}
-- convertTo1 = convert1 ; {-# INLINE convertTo1 #-}
-- convertTo2 = convert2 ; {-# INLINE convertTo2 #-}
-- convertTo3 = convert3 ; {-# INLINE convertTo3 #-}
-- convertTo4 = convert4 ; {-# INLINE convertTo4 #-}
-- convertTo5 = convert5 ; {-# INLINE convertTo5 #-}



-- -------------------------
-- -- === Convertibe' === --
-- -------------------------

-- -- === Definition === --



-- instance {-# OVERLAPPABLE #-} Convertible'  a a where convert'  = id ; {-# INLINE convert'  #-}
-- -- instance {-# OVERLAPPABLE #-} Convertible1' a (a t1) where convert1' = id ; {-# INLINE convert1' #-}
-- -- instance {-# OVERLAPPABLE #-} Convertible2' a (a t1 t2) where convert2' = id ; {-# INLINE convert2' #-}
-- -- instance {-# OVERLAPPABLE #-} Convertible3' a (a t1 t2 t3) where convert3' = id ; {-# INLINE convert3' #-}
-- -- instance {-# OVERLAPPABLE #-} Convertible4' a (a t1 t2 t3 t4) where convert4' = id ; {-# INLINE convert4' #-}
-- -- instance {-# OVERLAPPABLE #-} Convertible5' a (a t1 t2 t3 t4 t5) where convert5' = id ; {-# INLINE convert5' #-}

-- instance {-# OVERLAPPABLE #-} Convertible  a a' => Convertible'  a a' where convert'  = convert  ; {-# INLINE convert'  #-}
-- instance {-# OVERLAPPABLE #-} Convertible1 a a' => Convertible1' a a' where convert1' = convert1 ; {-# INLINE convert1' #-}
-- instance {-# OVERLAPPABLE #-} Convertible2 a a' => Convertible2' a a' where convert2' = convert2 ; {-# INLINE convert2' #-}
-- instance {-# OVERLAPPABLE #-} Convertible3 a a' => Convertible3' a a' where convert3' = convert3 ; {-# INLINE convert3' #-}
-- instance {-# OVERLAPPABLE #-} Convertible4 a a' => Convertible4' a a' where convert4' = convert4 ; {-# INLINE convert4' #-}
-- instance {-# OVERLAPPABLE #-} Convertible5 a a' => Convertible5' a a' where convert5' = convert5 ; {-# INLINE convert5' #-}

-- convertTo'  :: ∀ a' a. Convertible'  a a' =>              a           -> a'
-- convertTo1' :: ∀ a' a. Convertible1' a a' => ∀ b.         a b         -> a'
-- convertTo2' :: ∀ a' a. Convertible2' a a' => ∀ b c.       a b c       -> a'
-- convertTo3' :: ∀ a' a. Convertible3' a a' => ∀ b c d.     a b c d     -> a'
-- convertTo4' :: ∀ a' a. Convertible4' a a' => ∀ b c d e.   a b c d e   -> a'
-- convertTo5' :: ∀ a' a. Convertible5' a a' => ∀ b c d e f. a b c d e f -> a'
-- convertTo'  = convert'  ; {-# INLINE convertTo'  #-}
-- convertTo1' = convert1' ; {-# INLINE convertTo1' #-}
-- convertTo2' = convert2' ; {-# INLINE convertTo2' #-}
-- convertTo3' = convert3' ; {-# INLINE convertTo3' #-}
-- convertTo4' = convert4' ; {-# INLINE convertTo4' #-}
-- convertTo5' = convert5' ; {-# INLINE convertTo5' #-}




-- -------------------------------
-- -- === UnsafeConvertible === --
-- -------------------------------

-- -- === Definition === --

-- -- | UnsafeConvertible allows for conversion between two compatible types. When
-- --   trying to convert between the same types, compile time error is reported in
-- --   order to help tracking not needed usages. If you want to enable conversion
-- --   between the same types, use `convert'` instead.
-- class UnsafeConvertible  a a' where unsafeConvert  ::              a           -> a'
-- class UnsafeConvertible1 a a' where unsafeConvert1 :: ∀ b.         a b         -> a'
-- class UnsafeConvertible2 a a' where unsafeConvert2 :: ∀ b c.       a b c       -> a'
-- class UnsafeConvertible3 a a' where unsafeConvert3 :: ∀ b c d.     a b c d     -> a'
-- class UnsafeConvertible4 a a' where unsafeConvert4 :: ∀ b c d e.   a b c d e   -> a'
-- class UnsafeConvertible5 a a' where unsafeConvert5 :: ∀ b c d e f. a b c d e f -> a'


-- -- === Higher kind defaulting === --

-- instance {-# OVERLAPPABLE #-} UnsafeConvertible1 a a' => UnsafeConvertible  (a t) a' where unsafeConvert  = unsafeConvert1 ; {-# INLINE unsafeConvert  #-}
-- instance {-# OVERLAPPABLE #-} UnsafeConvertible2 a a' => UnsafeConvertible1 (a t) a' where unsafeConvert1 = unsafeConvert2 ; {-# INLINE unsafeConvert1 #-}
-- instance {-# OVERLAPPABLE #-} UnsafeConvertible3 a a' => UnsafeConvertible2 (a t) a' where unsafeConvert2 = unsafeConvert3 ; {-# INLINE unsafeConvert2 #-}
-- instance {-# OVERLAPPABLE #-} UnsafeConvertible4 a a' => UnsafeConvertible3 (a t) a' where unsafeConvert3 = unsafeConvert4 ; {-# INLINE unsafeConvert3 #-}
-- instance {-# OVERLAPPABLE #-} UnsafeConvertible5 a a' => UnsafeConvertible4 (a t) a' where unsafeConvert4 = unsafeConvert5 ; {-# INLINE unsafeConvert4 #-}


-- -- === Identity conversion errors === --

-- instance TypeError (IdConversionErr a) => UnsafeConvertible  a a where unsafeConvert  = impossible ; {-# INLINE unsafeConvert  #-}
-- instance TypeError (IdConversionErr a) => UnsafeConvertible1 a (a t1) where unsafeConvert1 = impossible ; {-# INLINE unsafeConvert1 #-}
-- instance TypeError (IdConversionErr a) => UnsafeConvertible2 a (a t1 t2) where unsafeConvert2 = impossible ; {-# INLINE unsafeConvert2 #-}
-- instance TypeError (IdConversionErr a) => UnsafeConvertible3 a (a t1 t2 t3) where unsafeConvert3 = impossible ; {-# INLINE unsafeConvert3 #-}
-- instance TypeError (IdConversionErr a) => UnsafeConvertible4 a (a t1 t2 t3 t4) where unsafeConvert4 = impossible ; {-# INLINE unsafeConvert4 #-}
-- instance TypeError (IdConversionErr a) => UnsafeConvertible5 a (a t1 t2 t3 t4 t5) where unsafeConvert5 = impossible ; {-# INLINE unsafeConvert5 #-}


-- -- === Utils === --

-- unsafeConvertTo  :: ∀ a' a. UnsafeConvertible  a a' =>              a           -> a'
-- unsafeConvertTo1 :: ∀ a' a. UnsafeConvertible1 a a' => ∀ b.         a b         -> a'
-- unsafeConvertTo2 :: ∀ a' a. UnsafeConvertible2 a a' => ∀ b c.       a b c       -> a'
-- unsafeConvertTo3 :: ∀ a' a. UnsafeConvertible3 a a' => ∀ b c d.     a b c d     -> a'
-- unsafeConvertTo4 :: ∀ a' a. UnsafeConvertible4 a a' => ∀ b c d e.   a b c d e   -> a'
-- unsafeConvertTo5 :: ∀ a' a. UnsafeConvertible5 a a' => ∀ b c d e f. a b c d e f -> a'
-- unsafeConvertTo  = unsafeConvert  ; {-# INLINE unsafeConvertTo  #-}
-- unsafeConvertTo1 = unsafeConvert1 ; {-# INLINE unsafeConvertTo1 #-}
-- unsafeConvertTo2 = unsafeConvert2 ; {-# INLINE unsafeConvertTo2 #-}
-- unsafeConvertTo3 = unsafeConvert3 ; {-# INLINE unsafeConvertTo3 #-}
-- unsafeConvertTo4 = unsafeConvert4 ; {-# INLINE unsafeConvertTo4 #-}
-- unsafeConvertTo5 = unsafeConvert5 ; {-# INLINE unsafeConvertTo5 #-}



-- -------------------------------
-- -- === UnsafeConvertibe' === --
-- -------------------------------

-- -- === Definition === --

-- class UnsafeConvertible'  a a' where unsafeConvert'  ::              a           -> a'
-- class UnsafeConvertible1' a a' where unsafeConvert1' :: ∀ b.         a b         -> a'
-- class UnsafeConvertible2' a a' where unsafeConvert2' :: ∀ b c.       a b c       -> a'
-- class UnsafeConvertible3' a a' where unsafeConvert3' :: ∀ b c d.     a b c d     -> a'
-- class UnsafeConvertible4' a a' where unsafeConvert4' :: ∀ b c d e.   a b c d e   -> a'
-- class UnsafeConvertible5' a a' where unsafeConvert5' :: ∀ b c d e f. a b c d e f -> a'

-- instance {-# OVERLAPPABLE #-} UnsafeConvertible'  a a where unsafeConvert'  = impossible ; {-# INLINE unsafeConvert'  #-}
-- instance {-# OVERLAPPABLE #-} UnsafeConvertible1' a (a t1) where unsafeConvert1' = impossible ; {-# INLINE unsafeConvert1' #-}
-- instance {-# OVERLAPPABLE #-} UnsafeConvertible2' a (a t1 t2) where unsafeConvert2' = impossible ; {-# INLINE unsafeConvert2' #-}
-- instance {-# OVERLAPPABLE #-} UnsafeConvertible3' a (a t1 t2 t3) where unsafeConvert3' = impossible ; {-# INLINE unsafeConvert3' #-}
-- instance {-# OVERLAPPABLE #-} UnsafeConvertible4' a (a t1 t2 t3 t4) where unsafeConvert4' = impossible ; {-# INLINE unsafeConvert4' #-}
-- instance {-# OVERLAPPABLE #-} UnsafeConvertible5' a (a t1 t2 t3 t4 t5) where unsafeConvert5' = impossible ; {-# INLINE unsafeConvert5' #-}

-- instance {-# OVERLAPPABLE #-} UnsafeConvertible  a a' => UnsafeConvertible'  a a' where unsafeConvert'  = unsafeConvert  ; {-# INLINE unsafeConvert'  #-}
-- instance {-# OVERLAPPABLE #-} UnsafeConvertible1 a a' => UnsafeConvertible1' a a' where unsafeConvert1' = unsafeConvert1 ; {-# INLINE unsafeConvert1' #-}
-- instance {-# OVERLAPPABLE #-} UnsafeConvertible2 a a' => UnsafeConvertible2' a a' where unsafeConvert2' = unsafeConvert2 ; {-# INLINE unsafeConvert2' #-}
-- instance {-# OVERLAPPABLE #-} UnsafeConvertible3 a a' => UnsafeConvertible3' a a' where unsafeConvert3' = unsafeConvert3 ; {-# INLINE unsafeConvert3' #-}
-- instance {-# OVERLAPPABLE #-} UnsafeConvertible4 a a' => UnsafeConvertible4' a a' where unsafeConvert4' = unsafeConvert4 ; {-# INLINE unsafeConvert4' #-}
-- instance {-# OVERLAPPABLE #-} UnsafeConvertible5 a a' => UnsafeConvertible5' a a' where unsafeConvert5' = unsafeConvert5 ; {-# INLINE unsafeConvert5' #-}

-- unsafeConvertTo'  :: ∀ a' a. UnsafeConvertible'  a a' =>              a           -> a'
-- unsafeConvertTo1' :: ∀ a' a. UnsafeConvertible1' a a' => ∀ b.         a b         -> a'
-- unsafeConvertTo2' :: ∀ a' a. UnsafeConvertible2' a a' => ∀ b c.       a b c       -> a'
-- unsafeConvertTo3' :: ∀ a' a. UnsafeConvertible3' a a' => ∀ b c d.     a b c d     -> a'
-- unsafeConvertTo4' :: ∀ a' a. UnsafeConvertible4' a a' => ∀ b c d e.   a b c d e   -> a'
-- unsafeConvertTo5' :: ∀ a' a. UnsafeConvertible5' a a' => ∀ b c d e f. a b c d e f -> a'
-- unsafeConvertTo'  = unsafeConvert'  ; {-# INLINE unsafeConvertTo'  #-}
-- unsafeConvertTo1' = unsafeConvert1' ; {-# INLINE unsafeConvertTo1' #-}
-- unsafeConvertTo2' = unsafeConvert2' ; {-# INLINE unsafeConvertTo2' #-}
-- unsafeConvertTo3' = unsafeConvert3' ; {-# INLINE unsafeConvertTo3' #-}
-- unsafeConvertTo4' = unsafeConvert4' ; {-# INLINE unsafeConvertTo4' #-}
-- unsafeConvertTo5' = unsafeConvert5' ; {-# INLINE unsafeConvertTo5' #-}



-- ----------------------------------
-- -- === Partial convertibles === --
-- ----------------------------------

-- -- === Errors === --

-- data SimpleConversionError = SimpleConversionError deriving (Show)
-- instance Default SimpleConversionError where def = SimpleConversionError ; {-# INLINE def #-}


-- -- === Classes === --

-- -- | PartialConvertible allows conversions that could fail with `ConversionError`.
-- class PartialConvertible t t' where
--     type family ConversionError t t'
--     convertAssert :: t -> Maybe (ConversionError t t')
--     unsafeConvertOld :: t -> t'

-- defConvertAssert :: Default e => (a -> Bool) -> a -> Maybe e
-- defConvertAssert f = \s -> if f s then Just def else Nothing

-- -- unsafeConvertTo :: ∀ t' t. PartialConvertible t t' => t -> t'
-- -- unsafeConvertTo = unsafeConvert ; {-# INLINE unsafeConvertTo #-}

-- convertAssertTo :: ∀ t' t. PartialConvertible t t' => t -> Maybe (ConversionError t t')
-- convertAssertTo = convertAssert @t @t' ; {-# INLINE convertAssertTo #-}

-- maybeConvert :: ∀ t t'. PartialConvertible t t' => t -> Maybe t'
-- maybeConvert t = const (unsafeConvertOld t) <$> convertAssertTo @t' t ; {-# INLINE maybeConvert #-}

-- tryConvert :: ∀ t t'. PartialConvertible t t' => t -> Either (ConversionError t t') t'
-- tryConvert t = maybe (Right $ unsafeConvertOld t) Left $ convertAssertTo @t' t ; {-# INLINE tryConvert #-}



-- -----------------------------
-- -- === Bi-convertibles === --
-- -----------------------------

-- type BiConvertible  t t' = (Convertible  t t', Convertible  t' t)
-- type BiConvertible'  t t' = (Convertible'  t t', Convertible'  t' t)


-- converted   :: BiConvertible   t t' =>                        Iso' t                  t'
-- converted'  :: BiConvertible'  t t' =>                        Iso' t                  t'
-- converted   = iso convert   convert   ; {-# INLINE converted   #-}
-- converted'  = iso convert'  convert'  ; {-# INLINE converted'  #-}

-- convertedTo   :: BiConvertible   t' t =>                        Iso' t                  t'
-- convertedTo'  :: BiConvertible'  t' t =>                        Iso' t                  t'
-- convertedTo   = converted   ; {-# INLINE convertedTo   #-}
-- convertedTo'  = converted'  ; {-# INLINE convertedTo'  #-}


-- -- === ConvertibleVia === --

-- type ConvertibleVia  t p t' = (Convertible  t p, Convertible  p t')

-- convertVia  :: ∀ p t t'. ConvertibleVia  t p t' =>                        t                -> t'
-- convertVia  = convert  . convertTo  @p ; {-# INLINE convertVia #-}

-- type ConvertibleVia'  t p t' = (Convertible'  t p, Convertible'  p t')

-- convertVia'  :: ∀ p t t'. ConvertibleVia'  t p t' =>                        t                -> t'
-- convertVia'  = convert'  . convertTo'  @p ; {-# INLINE convertVia' #-}

-- -- type PartialConvertibleVia  t p t' = (PartialConvertible  t p, PartialConvertible  p t')
-- -- unsafeConvertVia :: ∀ p t t'. PartialConvertibleVia t p t' => t -> t'
-- -- unsafeConvertVia = unsafeConvert . unsafeConvertTo @p ; {-# INLINE unsafeConvertVia #-}
