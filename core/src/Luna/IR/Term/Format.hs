{-# LANGUAGE TemplateHaskell #-}
module Luna.IR.Term.Format where

import Prologue

import qualified Data.Tag as Tag


Tag.family "Format" ["Literal", "Value", "Thunk", "Phrase", "Draft", "Ast"]

type family Of    elem   ::  Type
type family Elems format :: [Type]
