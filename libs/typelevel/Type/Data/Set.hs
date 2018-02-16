{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Data.Set where

import Prelude
import Data.Kind
import Type.Data.Bool
import Type.Data.Ord
import Type.Data.Ord.Class
import Data.Proxy (Proxy)
import Type.Data.Maybe (FromJust)


-----------------
-- === Set === --
-----------------

-- === Definition === --

data Set k = Set [k]

type family FromSet s where
    FromSet ('Set s) = s


-- === Operations === --

type Singleton t = 'Set '[t]

type family Insert k s where
    Insert k ('Set '[])       = 'Set '[k]
    Insert k ('Set (k ': as)) = 'Set (k ': as)
    Insert k ('Set (a ': as)) = If (k < a)
        ('Set (k ': a ': as))
        ('Set (a ': FromSet (Insert k ('Set as))))

type family Member k s where
    Member k ('Set '[])     = 'False
    Member k ('Set (k ': _)) = 'True
    Member k ('Set (_ ': s)) = Member k ('Set s)
