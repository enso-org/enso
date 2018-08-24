{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Data.Ord.Class where

import Prelude
import Type.Data.Bool

type family Cmp (a :: k) (b :: k) :: Ordering

type family a >  b where a >  b = Cmp a b == 'GT
type family a >= b where a >= b = Cmp a b != 'LT
type family a <  b where a <  b = Cmp a b == 'LT
type family a <= b where a <= b = Cmp a b != 'GT

