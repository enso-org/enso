{-# OPTIONS_GHC -fno-warn-orphans #-}

module Prologue.Data.Basic (module Prologue.Data.Basic, module X) where

import Data.Functor
import Prelude as X ( Bool(True,False), (&&), (||), not, otherwise
                    , Maybe(Just,Nothing), maybe
                    , Either(Left,Right), either
                    , Ordering(LT,EQ,GT)
                    , Char, String
                    , Int, Integer, Float, Double, Rational, Word
                    , Eq ((==), (/=))
                    , Ord (compare, (<), (<=), (>), (>=), max, min)
                    , fst, snd
                    )

-- === Utils === --

switch :: a -> a -> Bool -> a
switch ok fail cond = if cond then ok else fail ; {-# INLINE switch #-}

fromLeft :: (r -> l) -> Either l r -> l
fromLeft f = \case
    Right r -> f r
    Left  l -> l
{-# INLINE fromLeft #-}

fromRight :: (l -> r) -> Either l r -> r
fromRight f = \case
    Right r -> r
    Left  l -> f l
{-# INLINE fromRight #-}

infixr 2 ||.
infixr 2 &&.
(||.), (&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f ||. g = \s -> f s || g s ; {-# INLINE (||.) #-}
f &&. g = \s -> f s && g s ; {-# INLINE (&&.) #-}

const1 :: a -> (t1 -> a)
const2 :: a -> (t1 -> t2 -> a)
const3 :: a -> (t1 -> t2 -> t3 -> a)
const4 :: a -> (t1 -> t2 -> t3 -> t4 -> a)
const5 :: a -> (t1 -> t2 -> t3 -> t4 -> t5 -> a)
const6 :: a -> (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> a)
const7 :: a -> (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> a)
const8 :: a -> (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> a)
const9 :: a -> (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> a)
const1 a _ = a ; {-# INLINE const1 #-}
const2 a _ _ = a ; {-# INLINE const2 #-}
const3 a _ _ _ = a ; {-# INLINE const3 #-}
const4 a _ _ _ _ = a ; {-# INLINE const4 #-}
const5 a _ _ _ _ _ = a ; {-# INLINE const5 #-}
const6 a _ _ _ _ _ _ = a ; {-# INLINE const6 #-}
const7 a _ _ _ _ _ _ _ = a ; {-# INLINE const7 #-}
const8 a _ _ _ _ _ _ _ _ = a ; {-# INLINE const8 #-}
const9 a _ _ _ _ _ _ _ _ _ = a ; {-# INLINE const9 #-}


-- === Missing instances === --

deriving instance Functor ((,,) t1 t2)
deriving instance Functor ((,,,) t1 t2 t3)
deriving instance Functor ((,,,,) t1 t2 t3 t4)
deriving instance Functor ((,,,,,) t1 t2 t3 t4 t5)
deriving instance Functor ((,,,,,,) t1 t2 t3 t4 t5 t6)
deriving instance Functor ((,,,,,,,) t1 t2 t3 t4 t5 t6 t7)
deriving instance Functor ((,,,,,,,,) t1 t2 t3 t4 t5 t6 t7 t8)
deriving instance Functor ((,,,,,,,,,) t1 t2 t3 t4 t5 t6 t7 t8 t9)
