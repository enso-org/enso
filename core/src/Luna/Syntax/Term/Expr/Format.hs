{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Term.Expr.Format where

import Prelude.Luna hiding (String, Integer, Rational)

import Data.Container.Hetero (Elems)
import Type.List             (TakeUntil)

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

type instance Elems Literal = '[Star    , String    , Integer , Rational ]
type instance Elems Value   = '[Cons    , Lam                            ] <> Elems Literal
type instance Elems Thunk   = '[Acc     , App       , Curry   , Native   ] <> Elems Value
type instance Elems Phrase  = '[Blank   , Match     , Unify   , Var      ] <> Elems Thunk
type instance Elems Draft   = '[Missing                                  ] <> Elems Phrase
