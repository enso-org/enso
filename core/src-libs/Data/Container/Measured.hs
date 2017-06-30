{-# LANGUAGE UndecidableInstances #-}

module Data.Container.Measured where

import Prologue



----------------
-- Measurable --
----------------

type family SizeOf a
class Measurable a where
    size :: a -> SizeOf a


--------------
-- Measured --
--------------

-- Definition --

data Measured a = Measured !(SizeOf a) !a


-- Instances --

-- Show
deriving instance (Show (SizeOf a), Show a) => Show (Measured a)

-- Measurable
type instance SizeOf (Measured a) = SizeOf a
instance Measurable  (Measured a) where
    size (Measured s _) = s

-- Wrapped
instance Measurable a => Wrapped (Measured a) where
    type Unwrapped (Measured a) = a
    _Wrapped' = iso (\(Measured _ a) -> a) (\a -> Measured (size a) a)

-- Monoid
instance (Mempty a, Mempty (SizeOf a)) => Mempty (Measured a) where
    mempty = Measured mempty mempty

instance (Semigroup (SizeOf a), Semigroup a) => Semigroup (Measured a) where
    Measured s a <> Measured s' a' = Measured (s <> s') (a <> a')




-------------------
-- Measure types --
-------------------

newtype Length = Length Int64 deriving (Show, Num, Enum, Eq, Integral, Data, Ord, Read, Real, Ix)
makeWrapped ''Length

instance Mempty    Length where mempty = 0
instance Semigroup Length where (<>)   = (+)
