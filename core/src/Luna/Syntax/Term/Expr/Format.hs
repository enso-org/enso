module Luna.Syntax.Term.Expr.Format where

import Prelude.Luna


------------------------------
-- === Evaluation Model === --
------------------------------

-- === Definitions === --

data Lit    = Lit    deriving (Show)
data Val    = Val    deriving (Show)
data Thunk  = Thunk  deriving (Show)
data Phrase = Phrase deriving (Show)
data Draft  = Draft  deriving (Show)

type Formats = '[Lit, Val, Thunk, Phrase, Draft]
