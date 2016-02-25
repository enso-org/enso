{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Syntax.Model.Network.Builder.Term.Inferred (module Luna.Syntax.Model.Network.Builder.Term.Inferred, module X) where

import Prelude.Luna hiding (Num)

import           Type.Inference
import           Luna.Syntax.AST.Arg
import           Luna.Syntax.AST.Term
import           Data.Graph                                  (ELEMENT(..))
import qualified Luna.Syntax.Model.Network.Builder.Term.Class as Term
import           Luna.Syntax.Model.Network.Builder.Term.Class as X (arg)


---------------------
-- === Helpers === --
---------------------

-- === Types === --

type TermBuilderAs t (term :: k) m a = (Term.TermBuilder term m a, Inferable t a m)
type TermBuilder     (term :: k) m a = TermBuilderAs ELEMENT term m a


-- === Utils === --

inferNodeM :: Inferable ELEMENT t m => m t -> m t
inferNodeM = inferM ELEMENT



--------------------------------------------------------
-- === Term constructors inferred by explicit tag === --
--------------------------------------------------------

-- === Lit === --

starAs :: TermBuilderAs t Star m a => t -> m a
starAs t = inferM t Term.star

strAs :: TermBuilderAs t Str m a => t -> String -> m a
strAs t = inferM t ∘ Term.str

intAs :: TermBuilderAs t Num m a => t -> Int -> m a
intAs t = inferM t ∘ Term.int


-- === Val === --

consAs :: TermBuilderAs t Cons m a => t -> NameInput a -> m a
consAs t = inferM t ∘ Term.cons

lamAs :: TermBuilderAs t Lam m a => t -> [Arg $ Input a] -> Input a -> m a
lamAs t = inferM t ∘∘ Term.lam


-- === Thunk === --

accAs :: TermBuilderAs t Acc m a => t -> NameInput a -> Input a -> m a
accAs t = inferM t ∘∘ Term.acc

appAs :: TermBuilderAs t App m a => t -> Input a -> [Arg $ Input a] -> m a
appAs t = inferM t ∘∘ Term.app


-- === Expr === --

varAs :: TermBuilderAs t Var m a => t -> NameInput a -> m a
varAs t = inferM t ∘ Term.var

unifyAs :: TermBuilderAs t Unify m a => t -> Input a -> Input a -> m a
unifyAs t = inferM t ∘∘ Term.unify

subAs :: TermBuilderAs t Sub m a => t -> Input a -> Input a -> m a
subAs t = inferM t ∘∘ Term.sub


-- === Draft === --

blankAs :: TermBuilderAs t Blank m a => t -> m a
blankAs t = inferM t Term.blank



--------------------------------------------------------
-- === Term constructors inferred by implicit tag === --
--------------------------------------------------------

-- === Lit === --

star :: TermBuilder Star m a => m a
str  :: TermBuilder Str  m a => String -> m a
int  :: TermBuilder Num  m a => Int -> m a

star   = starAs ELEMENT
str    = strAs  ELEMENT
int    = intAs  ELEMENT

-- === Val === --

cons :: TermBuilder Cons m a => NameInput a -> m a
cons   = consAs  ELEMENT


-- === Thunk === --

acc :: TermBuilder Acc m a => NameInput a -> Input a -> m a
app :: TermBuilder App m a => Input a -> [Arg $ Input a] -> m a

acc    = accAs   ELEMENT
app    = appAs   ELEMENT


-- === Expr === --

var   :: TermBuilder Var   m a => NameInput a -> m a
unify :: TermBuilder Unify m a => Input a -> Input a -> m a
sub   :: TermBuilder Sub   m a => Input a -> Input a -> m a

var    = varAs   ELEMENT
unify  = unifyAs ELEMENT
sub    = subAs   ELEMENT


-- === Draft === --

blank :: TermBuilder Blank m a => m a
blank = blankAs ELEMENT
