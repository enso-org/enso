{-# LANGUAGE TemplateHaskell #-}
module Luna.IR.Term.Format where

import qualified Data.Tag as Tag

Tag.family "Format" ["Literal", "Value", "Thunk", "Phrase", "Draft", "Ast"]

