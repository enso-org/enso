{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}

module Old.Luna.Syntax.Model.Network.Builder.Node.Class (module Old.Luna.Syntax.Model.Network.Builder.Node.Class, module X) where

import Luna.Prelude

import           Old.Data.Graph
import           Old.Data.Graph.Builders
import           Old.Data.Record                             (ANY, Matches)
import           Luna.IR.Function.Argument
import           Old.Luna.Syntax.Term.Class                    hiding (Source)
import           Old.Luna.Syntax.Model.Layer
import qualified Old.Luna.Syntax.Model.Network.Builder.Term  as Term
import           Old.Luna.Syntax.Model.Network.Builder.Term  as X (arg, TermBuilder_OLD)
import           Type.Inference
import qualified Old.Luna.Syntax.Term.Expr.Lit                as Lit
import           Data.Layer_OLD.Cover_OLD


-- === Types === --

type NodeBuilder (t :: k) m node = TermBuilder_OLD t     m (Ref Node node)
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

type LitLike a = ( Matches (Uncovered a) '[ANY, Lit.Star, Lit.String, Lit.Number], Covered a)



-- === Utils === --

node :: n ~ Ref Node (ls :<: term) => m n -> m n
node = id ; {-# INLINE node #-}

--

star :: NodeBuilder Lit.Star m (ls :<: term) => m (Ref Node (ls :<: term))
star  = node Term.star

str :: NodeBuilder Lit.String m (ls :<: term) => String -> m (Ref Node (ls :<: term))
str   = node ∘ Term.str

ratio :: NodeBuilder Lit.Number m (ls :<: term) => Rational -> m (Ref Node (ls :<: term))
ratio = node ∘ Term.ratio

int :: NodeBuilder Lit.Number m (ls :<: term) => Integer -> m (Ref Node (ls :<: term))
int = node ∘ Term.int

double :: NodeBuilder Lit.Number m (ls :<: term) => Double -> m (Ref Node (ls :<: term))
double = node ∘ Term.double

number :: NodeBuilder Lit.Number m (ls :<: term) => Lit.Number -> m (Ref Node (ls :<: term))
number = node ∘ Term.number

cons :: NodeBuilder Cons m (ls :<: term) => NameInput (Ref Node (ls :<: term)) -> [Arg $ Ref Node (ls :<: term)] -> m (Ref Node (ls :<: term))
cons = node ∘∘ Term.cons

lam :: NodeBuilder Lam m (ls :<: term) => [Arg $ Ref Node (ls :<: term)] -> Ref Node (ls :<: term) -> m (Ref Node (ls :<: term))
lam = node ∘∘ Term.lam

acc :: NodeBuilder Acc m (ls :<: term) => NameInput (Ref Node (ls :<: term)) -> Ref Node (ls :<: term) -> m (Ref Node (ls :<: term))
acc = node ∘∘ Term.acc

app :: NodeBuilder App m (ls :<: term) => Ref Node (ls :<: term) -> [Arg $ Ref Node (ls :<: term)] -> m (Ref Node (ls :<: term))
app = node ∘∘ Term.app

curry :: NodeBuilder Curry m (ls :<: term) => Ref Node (ls :<: term) -> [Arg $ Ref Node (ls :<: term)] -> m (Ref Node (ls :<: term))
curry = node ∘∘ Term.curry

var :: NodeBuilder Var m (ls :<: term) => NameInput (Ref Node (ls :<: term)) -> m (Ref Node (ls :<: term))
var = Term.var

unify :: NodeBuilder Unify m (ls :<: term) => Ref Node (ls :<: term) -> Ref Node (ls :<: term) -> m (Ref Node (ls :<: term))
unify = Term.unify

match :: NodeBuilder Match m (ls :<: term) => Ref Node (ls :<: term) -> Ref Node (ls :<: term) -> m (Ref Node (ls :<: term))
match = Term.match

blank :: NodeBuilder Blank m (ls :<: term) => m (Ref Node (ls :<: term))
blank = Term.blank

native :: NodeBuilder Native m (ls :<: term) => NameInput (Ref Node (ls :<: term)) -> m (Ref Node (ls :<: term))
native = Term.native



-- star2 = node Term.star2
--
-- var2 = node ∘ Term.var2
