{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Term.Layout where

import qualified Data.Graph.Component.Layout as Layout
import qualified OCI.IR.Term.Class           as Term

import Data.Graph.Component.Layout ((:=), Layout)
import OCI.IR.Term.Class           (Terms)
import OCI.IR.Term.Layer.Model     (Model)


data Names

type instance Layout.ToLayout (Term.TermTag a) = Layout '[Model := (Term.TermTag a)]

type layout -* term = Layout.Set Terms term (Layout.ToLayout layout)
type layout -# name = Layout.Set Names name (Layout.ToLayout layout)
