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

switch :: a -> a -> Bool -> a
switch ok fail cond = if cond then ok else fail ; {-# INLINE switch #-}


-- === Missing instances === --

deriving instance Functor ((,,) t1 t2)
deriving instance Functor ((,,,) t1 t2 t3)
deriving instance Functor ((,,,,) t1 t2 t3 t4)
deriving instance Functor ((,,,,,) t1 t2 t3 t4 t5)
deriving instance Functor ((,,,,,,) t1 t2 t3 t4 t5 t6)
deriving instance Functor ((,,,,,,,) t1 t2 t3 t4 t5 t6 t7)
deriving instance Functor ((,,,,,,,,) t1 t2 t3 t4 t5 t6 t7 t8)
deriving instance Functor ((,,,,,,,,,) t1 t2 t3 t4 t5 t6 t7 t8 t9)
