{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}

module Luna.Syntax.Model.Network.Builder.Node.Class (module Luna.Syntax.Model.Network.Builder.Node.Class, module X) where

import Prelude.Luna

import           Data.Graph
import           Data.Graph.Builders
import           Data.Record                             (ANY, Matches)
import           Data.Graph.Builder.Ref
import           Luna.Syntax.AST.Arg
import qualified Luna.Syntax.AST.Term                    as Term
import           Luna.Syntax.AST.Term                    hiding (Val, Lit, Thunk, Expr, Draft, Source)
import           Luna.Syntax.Model.Layer
import qualified Luna.Syntax.Model.Network.Builder.Term  as Term
import           Luna.Syntax.Model.Network.Builder.Term  as X (arg, TermBuilder)
import           Type.Inference



-- === Types === --

type NodeBuilder (t :: k) m node = TermBuilder t     m (Ref Node node)
--type NodeReader           m node = Reader            m      (Node node)
--type NodeWriter           m node = Writer            m      (Node node)
type NodeLinkable         m node = Linkable            (Ref Node node) m
type NodeInferable        m node = Inferable ELEMENT   (Ref Node node) m

type InferredNodeBuilder (t :: k) m node = (NodeBuilder t m node, NodeInferable m node)

type TermNode (t :: k) m node = ( NodeBuilder t m node
                                , TermCtx       m node
                                )

type TermCtx m node = ( NodeLinkable   m node
                      --, NodeReader     m node
                      --, NodeWriter     m node
                      , Covered          node
                      , MonadFix       m
                      , IsString (NameInput (Ref Node node))
                      )

type LitLike a = ( Matches (Uncovered a) '[ANY, Star, Str, Term.Num], Covered a)



-- === Utils === --

node :: n ~ Ref Node (ls :<: term) => m n -> m n
node = id ; {-# INLINE node #-}

--

star :: NodeBuilder Star m (ls :<: term) => m (Ref Node $ ls :<: term)
star  = node Term.star

str :: NodeBuilder Str m (ls :<: term) => String -> m (Ref Node $ ls :<: term)
str   = node ∘ Term.str

int :: NodeBuilder Term.Num m (ls :<: term) => Int -> m (Ref Node $ ls :<: term)
int = node ∘ Term.int

cons :: NodeBuilder Cons m (ls :<: term) => NameInput (Ref Node $ ls :<: term) -> m (Ref Node $ ls :<: term)
cons = node ∘ Term.cons

lam :: NodeBuilder Lam m (ls :<: term) => [Arg $ Ref Node $ ls :<: term] -> Ref Node $ ls :<: term -> m (Ref Node $ ls :<: term)
lam = node ∘∘ Term.lam

acc :: NodeBuilder Acc m (ls :<: term) => NameInput (Ref Node $ ls :<: term) -> Ref Node $ ls :<: term -> m (Ref Node $ ls :<: term)
acc = node ∘∘ Term.acc

app :: NodeBuilder App m (ls :<: term) => Ref Node $ ls :<: term -> [Arg $ Ref Node $ ls :<: term] -> m (Ref Node $ ls :<: term)
app = node ∘∘ Term.app

var :: NodeBuilder Var m (ls :<: term) => NameInput (Ref Node $ ls :<: term) -> m (Ref Node $ ls :<: term)
var = Term.var

unify :: NodeBuilder Unify m (ls :<: term) => Ref Node $ ls :<: term -> Ref Node $ ls :<: term -> m (Ref Node $ ls :<: term)
unify = Term.unify

sub :: NodeBuilder Sub m (ls :<: term) => Ref Node $ ls :<: term -> Ref Node $ ls :<: term -> m (Ref Node $ ls :<: term)
sub = Term.sub

blank :: NodeBuilder Blank m (ls :<: term) => m (Ref Node $ ls :<: term)
blank = Term.blank

native :: NodeBuilder Native m (ls :<: term) => NameInput (Ref Node $ ls :<: term) -> [Ref Node $ ls :<: term] ->  m (Ref Node $ ls :<: term)
native = node ∘∘ Term.native
