{-# LANGUAGE UndecidableInstances #-}


module Luna.IR.Format (module Luna.IR.Format, module X) where

import Luna.Prelude hiding (String, Integer, Rational)

import Type.List                  (TakeUntil)

import Data.Property
import Type.Relation              (Super)
import Type.Bool
import Data.Reprx
import Type.Container (Every)
import Data.Families  (makeLunaComponents)
import OCI.IR.Layout.Format as X


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
    -- , "Definition"
    ]




type instance Super Literal    = '[]
type instance Super Value      = Literal ': Super Literal
type instance Super Thunk      = Value   ': Super Value
type instance Super Phrase     = Thunk   ': Super Thunk
type instance Super Draft      = Phrase  ': Super Phrase

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
