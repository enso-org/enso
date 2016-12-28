{-# LANGUAGE UndecidableInstances #-}
{-# BOOSTER  Templates            #-}

module Luna.IR.Expr.Atom where

import Luna.Prelude hiding (String, Integer, Rational, Curry)
import Data.Base
import Data.Phantom
import Data.Property
import Data.Reprx
import Type.Container (Every)
import Data.Families  (makeLunaComponents)
import Data.TypeVal


-- === Definition pragmas === --

makeLunaComponents "Atom" "Atomic"
    [ "Integer"
    , "Rational"
    , "String"
    , "Acc"
    , "App"
    , "Blank"
    , "Cons"
    , "Grouped"
    , "Lam"
    , "Missing"
    , "Star"
    , "Unify"
    , "Var"
    ]

type family AtomOf a ::  *
type family Atoms  a :: [*]


-- === AtomDesc === --

type AtomDesc = TypeDescT Atom

getAtomDesc :: forall a. KnownType (AtomOf a) => AtomDesc
getAtomDesc = getTypeDesc @(AtomOf a) ; {-# INLINE getAtomDesc #-}

atomDescOf :: forall a. KnownType (AtomOf a) => a -> AtomDesc
atomDescOf _ = getAtomDesc @a ; {-# INLINE atomDescOf #-}


-- === Instances === --

type instance AtomOf      (Atomic a) = Atomic a
type instance Atoms       (Atomic a) = '[Atomic a]
type instance Access Atom (Atomic a) = Atomic a
type instance TypeRepr    (Atomic a) = TypeRepr a
