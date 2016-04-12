module Luna.Syntax.Term.Expr.Format where

import Prelude.Luna


------------------------------
-- === Evaluation Model === --
------------------------------

-- === Definitions === --

data Literal = Literal deriving (Show)
data Value   = Value   deriving (Show)
data Thunk   = Thunk   deriving (Show)
data Phrase  = Phrase  deriving (Show)
data Draft   = Draft   deriving (Show)

type Formats = '[Literal, Value, Thunk, Phrase, Draft]
