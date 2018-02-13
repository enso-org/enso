module Luna.IR.Link where

import Prologue

import OCI.IR.Link
import qualified OCI.IR.Layout as Layout



------------------------
-- === Link types === --
------------------------


type Term t src = SubLinkRef (Layout.Rebase t src) Layout.Term
type Type t src = SubLinkRef (Layout.Rebase t src) Layout.Type
type Name t src = SubLinkRef (Layout.Rebase t src) Layout.Name
