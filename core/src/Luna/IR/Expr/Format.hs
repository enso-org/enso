{-# LANGUAGE UndecidableInstances #-}
{-# BOOSTER  Templates            #-}

module Luna.IR.Expr.Format where

import Luna.Prelude hiding (String, Integer, Rational)

import Type.List                  (TakeUntil)

import Luna.IR.Expr.Atom
import Data.Property
import Type.Relation              (Super)
import Type.Bool
import Data.Reprx
import Type.Container (Every)
import Data.Families  (makeLunaComponents)

--------------------------------
-- === Expression formats === --
--------------------------------

-- === Definitions === --

makeLunaComponents "Format" "Form"
    [ "Literal"
    , "Value"
    , "Thunk"
    , "Phrase"
    , "Draft"
    ]


-- === Selectors === --

data SuperFormats = SuperFormats deriving (Show)


-- === Instances === --

type instance TypeRepr (Form a) = TypeRepr a

-- === Relations === --

type instance Atoms Literal = '[Star    , String , Integer , Rational ]
type instance Atoms Value   = '[Cons    , Lam                         ] <> Atoms Literal
type instance Atoms Thunk   = '[Acc     , App    , Native             ] <> Atoms Value
type instance Atoms Phrase  = '[Blank   , Unify  , Var                ] <> Atoms Thunk
type instance Atoms Draft   = '[Missing                               ] <> Atoms Phrase

type instance Access Format Star     = Literal
type instance Access Format String   = Literal
type instance Access Format Integer  = Literal
type instance Access Format Rational = Literal
type instance Access Format Cons     = Value
type instance Access Format Lam      = Value
type instance Access Format Acc      = Thunk
type instance Access Format App      = Thunk
type instance Access Format Native   = Thunk
type instance Access Format Blank    = Phrase
type instance Access Format Unify    = Phrase
type instance Access Format Var      = Phrase
type instance Access Format Missing  = Draft


type instance Super Literal = '[]
type instance Super Value   = Literal ': Super Literal
type instance Super Thunk   = Value   ': Super Value
type instance Super Phrase  = Thunk   ': Super Thunk
type instance Super Draft   = Phrase  ': Super Phrase

-- === Sub-formats === --

type SubFormats a = TakeUntil a (Every Format)



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
