{-# LANGUAGE UndecidableInstances #-}

module Data.RTuple where

import Prelude
import Type.Container


-- === Definition === --

newtype RTuple (t :: [*]) = RTuple { fromRT :: Lst2RT t }


-- === Helpers === --

type family Lst2RT lst where
    Lst2RT '[]       = ()
    Lst2RT (a ': as) = (a, Lst2RT as)


-- === Instances === --

deriving instance Show (Lst2RT t) => Show (RTuple t)

type instance Concat (RTuple a) (RTuple b) = RTuple (a <> b)


-- === Operations === --

-- Merge
class Merge a b where merge :: a -> b -> a <> b
instance {-# OVERLAPPABLE #-} r ~ Concat '[] r                                                                        => Merge (RTuple '[])       (RTuple r) where merge _                r = r
instance {-# OVERLAPPABLE #-} (Lst2RT (Concat (l ': ls) r) ~ (l, Lst2RT (Concat ls r)), Merge (RTuple ls) (RTuple r)) => Merge (RTuple (l ': ls)) (RTuple r) where merge (RTuple (l, ls)) r = RTuple (l, fromRT $ merge (RTuple ls :: RTuple ls) r)


