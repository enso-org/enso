{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tuple.Utils.Class where

import Prologue

import Data.Tuple.Utils.TH



-------------------
-- === Tuple === --
-------------------

type family GetElemAt (n :: Nat) tup
type family SetElemAt (n :: Nat) v tup

class IxElemGetter (idx :: Nat) tup where
    getElemAt :: tup -> GetElemAt idx tup

class IxElemSetter (idx :: Nat) tup where
    setElemAt :: forall a. a -> tup -> SetElemAt idx a tup
