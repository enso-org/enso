{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Show where

import Prelude hiding (Show)

import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits


----------------------
-- === TypeShow === --
----------------------

-- === Definition === --

type family SymbolRepr (t :: k) :: Symbol where
    SymbolRepr a = ShowGeneric (Rep a)

type Show a = KnownSymbol (SymbolRepr a)

show :: ∀ a. Show a => String
show = symbolVal (Proxy @(SymbolRepr a)) ; {-# INLINE show #-}


-- === Instances === --

type family ShowGeneric t where
    ShowGeneric (M1 D ('MetaData name _ _ _) _) = name

