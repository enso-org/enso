{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Term.Layout where

import qualified Luna.IR.Component.Term.Class as Term
import qualified Data.Graph.Component.Layout                as Layout

import Luna.IR.Component.Term.Class       (Terms)
import Luna.IR.Component.Term.Layer.Model (Model)
import Data.Graph.Component.Layout                      ((:=), Layout)



data Names

type instance Layout.ToLayout (Term.TermTag a) = Layout '[Model := (Term.TermTag a)]

type layout -* term = Layout.Set Terms term (Layout.ToLayout layout)
type layout -# name = Layout.Set Names name (Layout.ToLayout layout)

