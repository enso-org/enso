{-# OPTIONS_GHC -fno-warn-orphans #-}

module Prologue.Data.Basic (module Prologue.Data.Basic, module X) where

import Prelude (Num, ($), (-), id)

import Data.Functor
import Data.Monoids
import Data.String.Class (IsString)
import Data.Foldable
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
import Control.Monad


-- === Utils === --

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a) ; {-# INLINE swap #-}

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


-- === General if-utils === --

ifThenElse   ::               Bool -> a -> a -> a
ifThenElseId ::               Bool -> (a -> a) -> (a -> a)
ifThenMempty :: (Mempty a) => Bool -> a -> a
ifThenElse   cond ok fl = if cond then ok else fl
ifThenElseId cond f     = if cond then f  else id
ifThenMempty cond ok    = if cond then ok else mempty
{-# INLINE ifThenElse   #-}
{-# INLINE ifThenElseId #-}
{-# INLINE ifThenMempty #-}

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM = \mcond ok fail -> mcond >>= (\cond -> if cond then ok else fail)
{-# INLINE ifM #-}

switch :: a -> a -> Bool -> a
switch = \ok fail cond -> ifThenElse cond ok fail
{-# INLINE switch #-}

switchM :: Monad m => m a -> m a -> m Bool -> m a
switchM = \ok fail mcond -> ifM mcond ok fail
{-# INLINE switchM #-}


-- === List-like manipulation === --

unlines :: (IsString a, Monoid a, Foldable f) => f a -> a
unlines = intercalate "\n" ; {-# INLINE unlines #-}

replicate, unsafeReplicate :: (Num a, Ord a) => a -> t -> [t]
replicate       i c = ifThenMempty (i>=0) $ unsafeReplicate i c ; {-# INLINE replicate #-}
unsafeReplicate i c = go i where
    go = \case 0 -> mempty
               j -> c : go (j - 1)
{-# INLINE unsafeReplicate #-}
