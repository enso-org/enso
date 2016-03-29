{-# LANGUAGE TypeFamilies #-}

module Prologue.Data.Tuple (module Prologue.Data.Tuple, module X) where

import           Control.Lens
import           Data.Tuple.OneTuple as X (OneTuple(OneTuple))
import           Data.Tuple.Curry    as X (Curry)
import qualified Data.Tuple.Curry    as Tuple


-- === Utils === --

curry   :: Curry a b => a -> b
uncurry :: Curry a b => b -> a
curry   = Tuple.curryN
uncurry = Tuple.uncurryN


-- === Missing instances === --

makeWrapped ''OneTuple

instance Curry (() -> r) r where
    curryN   f    = f () ; {-# INLINE curryN   #-}
    uncurryN f () = f    ; {-# INLINE uncurryN #-}
