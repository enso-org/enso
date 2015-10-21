{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module Data.Convert.Base where

import Control.Lens

----------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------

class MaybeConvertible a e b | a b -> e where
    tryConvert :: a -> Either e b

class MaybeConvertible' a e b | b -> a e where
    tryConvert' :: a -> Either e b

class Convertible a b where
    convert :: a -> b

-- Maybe we should change the name to Specializable and specialize?
class Convertible' a b | b -> a where
    convert' :: a -> b

class Castable a b where
    cast :: a -> b

type IsoMaybeConvertible  a e b = (MaybeConvertible  a e b, MaybeConvertible  b e a)
type IsoMaybeConvertible' a e b = (MaybeConvertible' a e b, MaybeConvertible' b e a)
type IsoConvertible       a b   = (Convertible       a b  , Convertible       b a  )
type IsoConvertible'      a b   = (Convertible'      a b  , Convertible'      b a  )
type IsoCastable          a b   = (Castable          a b  , Castable          b a  )

class ConvertibleM  m n where convertM  :: m (t1 :: k) -> n (t1 :: k)
class ConvertibleM2 m n where convertM2 :: m (t1 :: k) (t2 :: k) -> n (t1 :: k) (t2 :: k)
class ConvertibleM3 m n where convertM3 :: m (t1 :: k) (t2 :: k) (t3 :: k) -> n (t1 :: k) (t2 :: k) (t3 :: k)
class ConvertibleM4 m n where convertM4 :: m (t1 :: k) (t2 :: k) (t3 :: k) (t4 :: k) -> n (t1 :: k) (t2 :: k) (t3 :: k) (t4 :: k)
class ConvertibleM5 m n where convertM5 :: m (t1 :: k) (t2 :: k) (t3 :: k) (t4 :: k) (t5 :: k) -> n (t1 :: k) (t2 :: k) (t3 :: k) (t4 :: k) (t5 :: k)

-- utils

unsafeConvert :: Show e => MaybeConvertible a e b => a -> b
unsafeConvert a =
    case tryConvert a of
      Left  e -> error $ show e
      Right r -> r

casted :: IsoCastable a b => Iso' a b
casted = iso cast cast

converted :: IsoConvertible a b => Iso' a b
converted = iso convert convert

-- instances

instance {-# OVERLAPPABLE #-} Convertible a a where convert  = id

instance {-# OVERLAPPABLE #-} Convertible a b => Convertible (Maybe a) (Maybe b) where
    convert = fmap convert

instance {-# OVERLAPPABLE #-} Convertible a b => Castable a b where
    cast = convert