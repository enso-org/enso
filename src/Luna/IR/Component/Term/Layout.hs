{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Term.Layout where

import Prologue

import qualified Luna.IR.Component.Term.Class as Term
import qualified OCI.IR.Layout                as Layout

import Luna.IR.Component.Term.Class (Terms)
import Luna.IR.Component.Term.Layer (Model)
import OCI.IR.Layout                ((:=), Layout)




data Names

type instance Layout.ToLayout (Term.TermCons a) = Layout '[Model := (Term.TermCons a)]

type layout -* term = Layout.Set Terms term (Layout.ToLayout layout)
type layout -# name = Layout.Set Names name (Layout.ToLayout layout)

