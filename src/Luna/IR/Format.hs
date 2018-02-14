{-# LANGUAGE TemplateHaskell   #-}
module Luna.IR.Format where

import Prologue
import Data.Tag

import qualified OCI.IR.Layout as Layout
import OCI.IR.TH

$(tagFamily "Format" ["Literal", "Value", "Thunk", "Phrase", "Draft"])

type instance Layout.Rebase a (FormatTag _) = a
type instance Layout.GetSublayout t (FormatTag _) = Draft
