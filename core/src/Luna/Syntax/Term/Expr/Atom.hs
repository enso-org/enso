{-# LANGUAGE UndecidableInstances #-}
{-# BOOSTER  Templates            #-}

module Luna.Syntax.Term.Expr.Atom where

import Prelude.Luna hiding (String, Integer, Rational, Curry)
import Data.Base
import Data.Phantom
import Control.Lens.Property
import Data.Reprx

-- === Definition pragmas === --

define cname name = {{

data {cname}
type {name} = Atomic {cname}
type instance TypeRepr {cname} = "{name}"

}}

-- === Queries === --

data Atom = Atom deriving (Show)
type family Atoms a :: [*]


-- === Definitions === --

data Atomic a


-- === Instances === --

type instance Atoms    (Atomic a) = '[Atomic a]
type instance Get Atom (Atomic a) = Atomic a
type instance TypeRepr (Atomic a) = TypeRepr a

-- === Atoms === --

define INTEGER  Integer
define RATIONAL Rational
define STRING   String
define ACC      Acc
define APP      App
define BLANK    Blank
define CONS     Cons
define LAM      Lam
define MATCH    Match
define MISSING  Missing
define NATIVE   Native
define STAR     Star
define UNIFY    Unify
define VAR      Var
