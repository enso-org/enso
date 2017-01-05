module Luna.IR.Expr.Term.Uni where

import           Luna.Prelude hiding (Interger, Rational, String)
import qualified Luna.Prelude as Prelude

import Luna.IR.Function (Arg)
import Luna.IR.Expr.Term.Class (UniTerm, IsUniTerm)
import qualified Luna.IR.Expr.Layout as Layout
import qualified Luna.IR.Expr.Term.Named as Term
import qualified Luna.IR.Expr.Atom as Atom

data instance UniTerm (Layout.Named n a) = Integer  Prelude.Integer
                                         | Rational Prelude.Rational
                                         | String   Prelude.String
                                         | Acc      !n       !a
                                         | App      a        !(Arg a)
                                         | Lam      !(Arg a) !a
                                         | Unify    !a       !a
                                         | Cons      n
                                         | Var       n
                                         | Grouped   a
                                         | Blank
                                         | Star
                                         | Missing


deriving instance (Show n, Show a) => Show (UniTerm (Layout.Named n a))


instance IsUniTerm Atom.Integer  (Layout.Named n a) where uniTerm (Term.Sym_Integer  t1)    = Integer  t1
instance IsUniTerm Atom.Rational (Layout.Named n a) where uniTerm (Term.Sym_Rational t1)    = Rational t1
instance IsUniTerm Atom.String   (Layout.Named n a) where uniTerm (Term.Sym_String   t1)    = String   t1
instance IsUniTerm Atom.Acc      (Layout.Named n a) where uniTerm (Term.Sym_Acc      t1 t2) = Acc      t1 t2
instance IsUniTerm Atom.App      (Layout.Named n a) where uniTerm (Term.Sym_App      t1 t2) = App      t1 t2
instance IsUniTerm Atom.Lam      (Layout.Named n a) where uniTerm (Term.Sym_Lam      t1 t2) = Lam      t1 t2
instance IsUniTerm Atom.Unify    (Layout.Named n a) where uniTerm (Term.Sym_Unify    t1 t2) = Unify    t1 t2
instance IsUniTerm Atom.Cons     (Layout.Named n a) where uniTerm (Term.Sym_Cons     t1)    = Cons     t1
instance IsUniTerm Atom.Var      (Layout.Named n a) where uniTerm (Term.Sym_Var      t1)    = Var      t1
instance IsUniTerm Atom.Grouped  (Layout.Named n a) where uniTerm (Term.Sym_Grouped  t1)    = Grouped  t1
instance IsUniTerm Atom.Blank    (Layout.Named n a) where uniTerm  Term.Sym_Blank           = Blank
instance IsUniTerm Atom.Star     (Layout.Named n a) where uniTerm  Term.Sym_Star            = Star
instance IsUniTerm Atom.Missing  (Layout.Named n a) where uniTerm  Term.Sym_Missing         = Missing
