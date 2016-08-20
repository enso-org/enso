{-# LANGUAGE UndecidableInstances #-}
{-# BOOSTER  Templates            #-}

module Luna.Syntax.Term.Expr.Atom where

import Prelude.Luna hiding (String, Integer, Rational, Curry)
import Data.Base
import Data.Phantom
import Control.Lens.Property

-- === Definition pragmas === --

define name = {{

data Atom_{name}
type {name} = Atomic Atom_{name}

}}

-- === Queries === --

data Atom = Atom deriving (Show)
type family Atoms a :: [*]


-- === Definitions === --

data Atomic a


-- === Instances === --

type instance Atoms    (Atomic a) = '[Atomic a]
type instance Get Atom (Atomic a) = Atomic a


-- === Atoms === --

define Integer
define Rational
define String
define Acc
define App
define Blank
define Cons
define Curry
define Lam
define Match
define Missing
define Native
define Star
define Unify
define Var
