{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Data.Ord.Generic where

import GHC.Generics
import GHC.TypeLits   (CmpSymbol)
import Prelude
import Type.Data.Bool


type Cmp a b = Cmp__ (GenericPath__ (Rep a)) (GenericPath__ (Rep b))

type family GenericPath__ a where
    GenericPath__ (M1 D ('MetaData name mod pkg _) _) = '(pkg, mod, name)

type family Cmp__ a b where
    Cmp__ '(pkg, mod, name) '(pkg', mod', name')
        = Resolve__ (CmpSymbol pkg  pkg')
                    (CmpSymbol mod  mod')
                    (CmpSymbol name name')

type family Resolve__ a b c where
    Resolve__ 'EQ 'EQ a = a
    Resolve__ 'EQ a   _ = a
    Resolve__ a   _   _ = a


type family a >  b where a >  b = Cmp a b == 'GT
type family a >= b where a >= b = Cmp a b != 'LT
type family a <  b where a <  b = Cmp a b == 'LT
type family a <= b where a <= b = Cmp a b != 'GT

