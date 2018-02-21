{-# LANGUAGE TemplateHaskell   #-}
module Luna.IR.Format where

import Prologue

import qualified Data.Tag      as Tag
import qualified OCI.IR.Layout as Layout


Tag.family "Format" ["Literal", "Value", "Thunk", "Phrase", "Draft"]

type instance Layout.Rebase a (FormatTag _) = a
type instance Layout.GetSublayout t (FormatTag _) = Draft
