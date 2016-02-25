{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Syntax.Model.Network.Builder.Node.Inferred where

import Prelude.Luna

import qualified Luna.Syntax.Model.Network.Builder.Node.Class as Node
import Luna.Syntax.Model.Network.Builder.Term.Inferred (inferNodeM)


star   = inferNodeM    Node.star
str    = inferNodeM ∘  Node.str
int    = inferNodeM ∘  Node.int
cons   = inferNodeM ∘  Node.cons
lam    = inferNodeM ∘∘ Node.lam
acc    = inferNodeM ∘∘ Node.acc
app    = inferNodeM ∘∘ Node.app
var    = inferNodeM ∘  Node.var
unify  = inferNodeM ∘∘ Node.unify
sub    = inferNodeM ∘∘ Node.sub
blank  = inferNodeM    Node.blank
native = inferNodeM ∘∘ Node.native
