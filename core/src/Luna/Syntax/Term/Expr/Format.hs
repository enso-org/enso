{-# LANGUAGE UndecidableInstances #-}
{-# BOOSTER  Templates            #-}

module Luna.Syntax.Term.Expr.Format where

import Prelude.Luna hiding (String, Integer, Rational)

import Type.List                  (TakeUntil)

import Luna.Syntax.Term.Expr.Atom
import Control.Lens.Property
import Type.Relation              (Super)
import Type.Bool
import Data.Reprx

--------------------------------
-- === Expression formats === --
--------------------------------

-- === Definition pragmas === --

define name = {{

data Form_{name}
type {name} = Form Form_{name}
type instance TypeRepr Form_{name} = "{name}"

}}


-- === Definitions === --

data Form a
type Formats = '[Literal, Value, Thunk, Phrase, Draft]

define Literal
define Value
define Thunk
define Phrase
define Draft


-- === Selectors === --

data Format       = Format       deriving (Show)
data SuperFormats = SuperFormats deriving (Show)


-- === Instances === --

type instance TypeRepr (Form a) = TypeRepr a

-- === Relations === --

type instance Atoms Literal = '[Star    , String    , Integer , Rational ]
type instance Atoms Value   = '[Cons    , Lam                            ] <> Atoms Literal
type instance Atoms Thunk   = '[Acc     , App       , Native             ] <> Atoms Value
type instance Atoms Phrase  = '[Blank   , Match     , Unify   , Var      ] <> Atoms Thunk
type instance Atoms Draft   = '[Missing                                  ] <> Atoms Phrase

type instance Get Format Star     = Literal
type instance Get Format String   = Literal
type instance Get Format Integer  = Literal
type instance Get Format Rational = Literal
type instance Get Format Cons     = Value
type instance Get Format Lam      = Value
type instance Get Format Acc      = Thunk
type instance Get Format App      = Thunk
type instance Get Format Native   = Thunk
type instance Get Format Blank    = Phrase
type instance Get Format Match    = Phrase
type instance Get Format Unify    = Phrase
type instance Get Format Var      = Phrase
type instance Get Format Missing  = Draft


type instance Super Literal = '[]
type instance Super Value   = Literal ': Super Literal
type instance Super Thunk   = Value   ': Super Value
type instance Super Phrase  = Thunk   ': Super Thunk
type instance Super Draft   = Phrase  ': Super Phrase

-- === Sub-formats === --

type SubFormats a = TakeUntil a Formats

type family   Sub t a
type instance Sub t (Form   f) = Form   f
type instance Sub t (Atomic a) = Atomic a

-- TODO: automatize:

type instance Literal > Literal = False
type instance Literal > Value   = False
type instance Literal > Thunk   = False
type instance Literal > Phrase  = False
type instance Literal > Draft   = False

type instance Value > Literal = True
type instance Value > Value   = False
type instance Value > Thunk   = False
type instance Value > Phrase  = False
type instance Value > Draft   = False

type instance Thunk > Literal = True
type instance Thunk > Value   = True
type instance Thunk > Thunk   = False
type instance Thunk > Phrase  = False
type instance Thunk > Draft   = False

type instance Phrase > Literal = True
type instance Phrase > Value   = True
type instance Phrase > Thunk   = True
type instance Phrase > Phrase  = False
type instance Phrase > Draft   = False

type instance Draft > Literal = True
type instance Draft > Value   = True
type instance Draft > Thunk   = True
type instance Draft > Phrase  = True
type instance Draft > Draft   = False
