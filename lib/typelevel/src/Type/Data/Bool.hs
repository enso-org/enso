{-# LANGUAGE NoStrict      #-}
{-# LANGUAGE NoStrictData  #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Type.Data.Bool where

import Prelude

import GHC.Exts (Constraint)


-- === Basic operations ===

type family Not a where
    Not 'True  = 'False
    Not 'False = 'True

infixr 3 &&
type a && b = And a b
type family   And a b where
    And 'True 'True = 'True
    And a      b    = 'False

type family Or a b where
    Or 'True b     = 'True
    Or a     'True = 'True
    Or a     b     = 'False

type family Xor a b where
    Xor 'True 'True = 'False
    Xor 'True b     = 'True
    Xor a     'True = 'True
    Xor a     b     = 'False

type family If (cond :: Bool) (a :: k) (b :: k) :: k where
    If cond   a a = a
    If 'True  a b = a
    If 'False a b = b

type family If' (cond :: Bool) (a :: Constraint) (b :: Constraint) :: Constraint
    where
        If'  cond  a a = a
        If' 'True  a b = a
        If' 'False a b = b

type family (a :: k) == (b :: k) where
    a == a = 'True
    a == b = 'False

type a != b = Not (a == b)

type a || b = Or a b

