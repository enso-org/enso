{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Term.Expr.Format where

import Prelude.Luna hiding (String, Integer, Rational)

import Luna.Syntax.Term.Expr.Atom (Atoms)
import Type.List                  (TakeUntil)

import Luna.Syntax.Term.Expr.Atom


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


-- === Utils === --

type SubFormats a = TakeUntil a Formats


-- === Relations === --

type instance Atoms Literal = '[Star    , String    , Integer , Rational ]
type instance Atoms Value   = '[Cons    , Lam                            ] <> Atoms Literal
type instance Atoms Thunk   = '[Acc     , App       , Curry   , Native   ] <> Atoms Value
type instance Atoms Phrase  = '[Blank   , Match     , Unify   , Var      ] <> Atoms Thunk
type instance Atoms Draft   = '[Missing                                  ] <> Atoms Phrase
