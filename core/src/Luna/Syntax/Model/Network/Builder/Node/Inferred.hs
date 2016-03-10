{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Syntax.Model.Network.Builder.Node.Inferred where

import Prelude.Luna

import qualified Luna.Syntax.Model.Network.Builder.Node.Class as Node
import Luna.Syntax.Model.Network.Builder.Term.Inferred (inferNodeM)


star   = inferNodeM    Node.star
str    = inferNodeM ∘  Node.str
int    = inferNodeM ∘  Node.int
double = inferNodeM ∘  Node.double
cons   = inferNodeM ∘∘ Node.cons
lam    = inferNodeM ∘∘ Node.lam
acc    = inferNodeM ∘∘ Node.acc
app    = inferNodeM ∘∘ Node.app
var    = inferNodeM ∘  Node.var
unify  = inferNodeM ∘∘ Node.unify
match  = inferNodeM ∘∘ Node.match
blank  = inferNodeM    Node.blank
native = inferNodeM ∘∘ Node.native
