{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds                 #-}

module Luna.Syntax.Model.Network.Builder.Term.Inferred (module Luna.Syntax.Model.Network.Builder.Term.Inferred, module X) where

import Prelude.Luna hiding (Num)

import           Data.Graph                                   (ELEMENT (..))
import           Luna.Syntax.Model.Network.Builder.Term.Class as X (arg)
import qualified Luna.Syntax.Model.Network.Builder.Term.Class as Term
import           Luna.Syntax.Term.Expr
import           Luna.Syntax.Term.Function.Argument
import qualified Luna.Syntax.Term.Lit                         as Lit
import           Type.Inference

---------------------
-- === Helpers === --
---------------------

-- === Types === --

type TermBuilderAs_OLD t (term :: k) m a = (Term.TermBuilder_OLD term m a, Inferable t a m)
type TermBuilder_OLD     (term :: k) m a = TermBuilderAs_OLD ELEMENT term m a


-- === Utils === --

inferNodeM :: Inferable ELEMENT t m => m t -> m t
inferNodeM = inferM ELEMENT



--------------------------------------------------------
-- === Term constructors inferred by explicit tag === --
--------------------------------------------------------

-- === Lit === --

starAs :: TermBuilderAs_OLD t Lit.Star m a => t -> m a
starAs t = inferM t Term.star

strAs :: TermBuilderAs_OLD t Lit.String m a => t -> String -> m a
strAs t = inferM t ∘ Term.str

ratioAs :: TermBuilderAs_OLD t Lit.Number m a => t -> Rational -> m a
ratioAs t = inferM t ∘ Term.ratio

intAs :: TermBuilderAs_OLD t Lit.Number m a => t -> Integer -> m a
intAs t = inferM t ∘ Term.int

doubleAs :: TermBuilderAs_OLD t Lit.Number m a => t -> Double -> m a
doubleAs t = inferM t ∘ Term.double

numberAs :: TermBuilderAs_OLD t Lit.Number m a => t -> Lit.Number -> m a
numberAs t = inferM t ∘ Term.number

-- === Val === --

consAs :: TermBuilderAs_OLD t Cons m a => t -> NameInput a -> [Arg $ Input a] -> m a
consAs t = inferM t ∘∘ Term.cons

lamAs :: TermBuilderAs_OLD t Lam m a => t -> [Arg $ Input a] -> Input a -> m a
lamAs t = inferM t ∘∘ Term.lam


-- === Thunk === --

accAs :: TermBuilderAs_OLD t Acc m a => t -> NameInput a -> Input a -> m a
accAs t = inferM t ∘∘ Term.acc

appAs :: TermBuilderAs_OLD t App m a => t -> Input a -> [Arg $ Input a] -> m a
appAs t = inferM t ∘∘ Term.app


-- === Expr === --

varAs :: TermBuilderAs_OLD t Var m a => t -> NameInput a -> m a
varAs t = inferM t ∘ Term.var

unifyAs :: TermBuilderAs_OLD t Unify m a => t -> Input a -> Input a -> m a
unifyAs t = inferM t ∘∘ Term.unify

subAs :: TermBuilderAs_OLD t Match m a => t -> Input a -> Input a -> m a
subAs t = inferM t ∘∘ Term.match


-- === Draft === --

blankAs :: TermBuilderAs_OLD t Blank m a => t -> m a
blankAs t = inferM t Term.blank



--------------------------------------------------------
-- === Term constructors inferred by implicit tag === --
--------------------------------------------------------

-- === Lit === --

star   :: TermBuilder_OLD Lit.Star   m a => m a
str    :: TermBuilder_OLD Lit.String m a => String     -> m a
ratio  :: TermBuilder_OLD Lit.Number m a => Rational   -> m a
int    :: TermBuilder_OLD Lit.Number m a => Integer    -> m a
double :: TermBuilder_OLD Lit.Number m a => Double     -> m a
number :: TermBuilder_OLD Lit.Number m a => Lit.Number -> m a

star   = starAs   ELEMENT
str    = strAs    ELEMENT
ratio  = ratioAs  ELEMENT
int    = intAs    ELEMENT
double = doubleAs ELEMENT
number = numberAs ELEMENT

-- === Val === --

cons :: TermBuilder_OLD Cons m a => NameInput a -> [Arg $ Input a] -> m a
cons   = consAs  ELEMENT


-- === Thunk === --

acc :: TermBuilder_OLD Acc m a => NameInput a -> Input a -> m a
app :: TermBuilder_OLD App m a => Input a -> [Arg $ Input a] -> m a

acc    = accAs   ELEMENT
app    = appAs   ELEMENT


-- === Expr === --

var   :: TermBuilder_OLD Var   m a => NameInput a -> m a
unify :: TermBuilder_OLD Unify m a => Input a -> Input a -> m a
match :: TermBuilder_OLD Match m a => Input a -> Input a -> m a

var    = varAs   ELEMENT
unify  = unifyAs ELEMENT
match    = subAs   ELEMENT


-- === Draft === --

blank :: TermBuilder_OLD Blank m a => m a
blank = blankAs ELEMENT
