module Luna.IR.Link where

import Prologue

import OCI.IR.Link
import OCI.IR.Layout



------------------------
-- === Link types === --
------------------------


data TERM
data TYPE
data NAME

type Term t src = SubLinkRef (Rebase t src) TERM
type Type t src = SubLinkRef (Rebase t src) TYPE
type Name t src = SubLinkRef (Rebase t src) NAME
