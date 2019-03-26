{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Data.Set where

import Prelude

import Type.Data.Bool
import Type.Data.Ord


-----------------
-- === Set === --
-----------------

-- === Definition === --

newtype Set k = Set [k]

type family FromSet s where
    FromSet ('Set s) = s


-- === Bindings to raw operations === --

type Singleton t = 'Set (RawSingleton t)
type family Insert k s where Insert k ('Set s) = 'Set (RawInsert k s)
type family Member k s where Member k ('Set s) = RawMember k s


-- === Raw list operations === --

type RawSingleton t = '[t]

type family RawInsert k s where
    RawInsert k '[]       = '[k]
    RawInsert k (k ': as) = (k ': as)
    RawInsert k (a ': as) = RawSubInsert (k < a) k a as

type family RawSubInsert b k a as where
    RawSubInsert b k a as = If b
        (k ': a ': as)
        (a ': (RawInsert k as))

type family RawMember k s where
    RawMember k '[]     = 'False
    RawMember k (k ': _) = 'True
    RawMember k (_ ': s) = RawMember k s

