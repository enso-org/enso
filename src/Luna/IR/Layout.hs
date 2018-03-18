{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Layout where

import Prologue

import qualified Luna.IR.Term.Class as Term
import qualified OCI.IR.Layout      as Layout

import Luna.IR.Term.Class (Model, Terms)
import OCI.IR.Layout      ((:=), Layout)




data Names

type instance Layout.ToLayout (Term.TermCons a) = Layout '[Model := (Term.TermCons a)]

type layout -* term = Layout.Set Terms term (Layout.ToLayout layout)
type layout -# name = Layout.Set Names name (Layout.ToLayout layout)
