{-# LANGUAGE UndecidableInstances #-}
{-# BOOSTER  Templates            #-}

module Luna.Syntax.Term.Expr.Format where

import Prelude.Luna hiding (String, Integer, Rational)

import Type.List                  (TakeUntil)

import Luna.Syntax.Term.Expr.Atom
import Control.Lens.Property
import Type.Relation              (Super)
import Type.Bool

--------------------------------
-- === Expression formats === --
--------------------------------

-- === Definition pragmas === --

define name = {{

data Form_{name}
type {name} = Form Form_{name}

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

-- === Sub-formats === --

type family Sub t a

type instance Sub t (Form   f) = Form   f
type instance Sub t (Atomic a) = Atomic a

-- === Merging === --

data a :> b
type family Merge a b

type family Simplify l where
    Simplify (a :> ()) = a
    Simplify (a :> a)  = a
    Simplify a         = a

type instance Merge (Form   a) (Form   b) = If (Form a > Form b) (Form a) (Form b)
type instance Merge (Form   a) (Atomic b) = Merge (Form a) (Atomic b ^. Format)
type instance Merge (Atomic a) (Form   b) = Merge (Atomic a ^. Format) (Form b)
type instance Merge (Atomic a) (Atomic b) = If (a == b) (Atomic a) (Merge (Atomic a ^. Format) (Atomic b ^. Format))-- TODO: refactor
type instance Merge (Form   a) ()         = Form a
type instance Merge ()         (Form a  ) = Form a
type instance Merge (Atomic a) ()         = Atomic a
type instance Merge ()         (Atomic a) = Atomic a
type instance Merge (t :> s)   ()         = t :> s
type instance Merge ()         (t :> s)   = t :> s
type instance Merge ()         ()         = ()

type instance Merge (t :> s)   (t' :> s') = Simplify (Merge t t' :> Merge s s')
type instance Merge (t :> s)   (Atomic a) = Simplify (Merge t (Atomic a) :> Merge s (Atomic a))
type instance Merge (Atomic a) (t :> s)   = Simplify (Merge (Atomic a) t :> Merge (Atomic a) s)
type instance Merge (t :> s)   (Form a)   = Simplify (Merge t (Form a) :> Merge s (Form a))
type instance Merge (Form a)   (t :> s)   = Simplify (Merge (Form a) t :> Merge (Form a) s)

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
