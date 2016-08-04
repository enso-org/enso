{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Term.Expr.Format where

import Prelude.Luna hiding (String, Integer, Rational)

import Luna.Syntax.Term.Expr.Atom (Atoms)
import Type.List                  (TakeUntil)

import Luna.Syntax.Term.Expr.Atom
import Control.Lens.Property
import Type.Relation              (Super)

--------------------------------
-- === Expression formats === --
--------------------------------

-- === Definitions === --

data Literal = Literal deriving (Show)
data Value   = Value   deriving (Show)
data Thunk   = Thunk   deriving (Show)
data Phrase  = Phrase  deriving (Show)
data Draft   = Draft   deriving (Show)

data Format  = Format deriving (Show)
type Formats = '[Literal, Value, Thunk, Phrase, Draft]

-- === Selectors === --

data SuperFormats = SuperFormats deriving (Show)


-- === Utils === --

type SubFormats a = TakeUntil a Formats


-- === Relations === --

type instance Atoms Literal = '[Star    , String    , Integer , Rational ]
type instance Atoms Value   = '[Cons    , Lam                            ] <> Atoms Literal
type instance Atoms Thunk   = '[Acc     , App       , Curry   , Native   ] <> Atoms Value
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
type instance Get Format Curry    = Thunk
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
