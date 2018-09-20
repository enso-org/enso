{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE TypeInType #-}

module Data.Property where

import Data.Kind


----------------------
-- === Property === --
----------------------


-- === Definition === --

type family Get (key :: Type) (obj :: k) :: Type
type family Set (key :: Type) (val :: Type) (obj :: k) :: k


-- === Operators === --

infixl 8 #
type obj # key = Get key obj


-- === Invariants === --

type GetSetInvariant key obj = Set key (Get key obj) obj ~ obj

