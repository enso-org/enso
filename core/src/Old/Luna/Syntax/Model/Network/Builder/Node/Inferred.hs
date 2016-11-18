{-# LANGUAGE NoMonomorphismRestriction #-}

module Old.Luna.Syntax.Model.Network.Builder.Node.Inferred where

import Luna.Prelude

import qualified Old.Luna.Syntax.Model.Network.Builder.Node.Class as Node
import Old.Luna.Syntax.Model.Network.Builder.Term.Inferred (inferNodeM)
import Data.Graph
import Old.Luna.Syntax.Model.Layer
import qualified Old.Luna.Syntax.Term.Expr.Lit                as Lit
import           Type.Inference (Inferable)

star :: (Node.NodeBuilder Lit.Star m (ls :<: term), Inferable ELEMENT (Ref Node (ls :<: term)) m) => m (Ref Node (ls :<: term))
star   = inferNodeM    Node.star
str    = inferNodeM ∘  Node.str
int    = inferNodeM ∘  Node.int
double = inferNodeM ∘  Node.double
number = inferNodeM ∘  Node.number
lam    = inferNodeM ∘∘ Node.lam
app    = inferNodeM ∘∘ Node.app
curry  = inferNodeM ∘∘ Node.curry
unify  = inferNodeM ∘∘ Node.unify
match  = inferNodeM ∘∘ Node.match
blank  = inferNodeM    Node.blank

cons   = inferNodeM ∘∘ Node.cons
acc    = inferNodeM ∘∘ Node.acc
var    = inferNodeM ∘  Node.var
native = inferNodeM ∘  Node.native


--inferNodeM :: Inferable ELEMENT t m => m t -> m t
--star :: NodeBuilder Lit.Star m (ls :<: term) => m (Ref Node (ls :<: term))
--cons :: NodeBuilder Cons m (ls :<: term) => NameInput (Ref Node (ls :<: term)) -> [Arg $ Ref Node (ls :<: term)] -> m (Ref Node (ls :<: term))
