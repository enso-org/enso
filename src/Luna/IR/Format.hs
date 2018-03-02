{-# LANGUAGE TemplateHaskell   #-}
module Luna.IR.Format where

import Prologue

import qualified Data.Tag      as Tag
import qualified OCI.IR.Layout2 as Layout


Tag.family "Format" ["Literal", "Value", "Thunk", "Phrase", "Draft"]

-- type instance Layout.Rebase a (Format _) = a
-- type instance Layout.GetSublayout t (Format _) = Draft

type instance Layout.Branch _ (Format _) = Layout.EmptyBranch Draft
