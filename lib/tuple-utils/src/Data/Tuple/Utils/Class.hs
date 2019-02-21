{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tuple.Utils.Class where

import Prologue hiding (head, tail)



-------------------
-- === Tuple === --
-------------------

type family GetElemAt (n :: Nat) tup
type family SetElemAt (n :: Nat) v tup

class IxElemGetter (idx :: Nat) tup where
    getElemAt :: tup -> GetElemAt idx tup

class IxElemSetter (idx :: Nat) tup where
    setElemAt :: forall a. a -> tup -> SetElemAt idx a tup

class Prependable t tup where
    prepend :: t -> tup -> Prepended t tup

type family Prepended (t :: Type) (tup :: Type) :: Type

type family Tail (tup :: Type) :: Type
type family Head (tup :: Type) :: Type

class TailGetter tup where
    tail :: tup -> Tail tup

class HeadGetter tup where
    head :: tup -> Head tup

splitHead :: (HeadGetter tup, TailGetter tup)
          => tup -> (Head tup, Tail tup)
splitHead = \tup -> (head tup, tail tup)
{-# INLINE splitHead #-}

