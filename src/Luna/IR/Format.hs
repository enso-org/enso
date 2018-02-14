module Luna.IR.Format where

import Prologue
import Data.Tag

import qualified OCI.IR.Layout as Layout

data FORMAT
type FormatTag = Tag FORMAT


data LITERAL ; type Literal = FormatTag LITERAL
data VALUE   ; type Value   = FormatTag VALUE
data THUNK   ; type Thunk   = FormatTag THUNK
data PHRASE  ; type Phrase  = FormatTag PHRASE
data DRAFT   ; type Draft   = FormatTag DRAFT



type instance Layout.Rebase a (FormatTag _) = a
type instance Layout.GetSublayout t (FormatTag _) = Draft
