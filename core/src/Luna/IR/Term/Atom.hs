{-# LANGUAGE UndecidableInstances #-}
{-# BOOSTER  Templates            #-}

module Luna.IR.Term.Atom where

import Luna.Prelude hiding (String, Integer, Rational, Curry)
import Data.Base
import Data.Phantom
import Data.Property
import Data.Reprx
import Type.Container (Every)
import Data.Families  (makeLunaComponents)


-- === Definition pragmas === --

makeLunaComponents "Atom" "Atomic"
    [ "Integer"
    , "Rational"
    , "String"
    , "Acc"
    , "App"
    , "Blank"
    , "Cons"
    , "Lam"
    , "Match"
    , "Missing"
    , "Native"
    , "Star"
    , "Unify"
    , "Var"
    ]

type family Atoms a :: [*]

-- === Instances === --

type instance Atoms       (Atomic a) = '[Atomic a]
type instance Access Atom (Atomic a) = Atomic a
type instance TypeRepr    (Atomic a) = TypeRepr a
