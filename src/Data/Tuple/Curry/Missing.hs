module Data.Tuple.Curry.Missing (module Data.Tuple.Curry.Missing, module X) where

import Data.Tuple.Curry as X

instance Curry (() -> r) r where
    curryN   f     = f ()
    uncurryN f ~() = f
