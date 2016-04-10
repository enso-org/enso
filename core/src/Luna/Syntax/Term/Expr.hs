module Luna.Syntax.Term.Expr where

import Prelude.Luna

import Data.Base                 (Base)
import Data.Construction         (Args, DataType (args))
import Luna.Runtime.Dynamics     (ByDynamics)
import Luna.Syntax.Term.Function (Arg)
import Type.Applicative

import qualified Luna.Syntax.Term.Atom as Atom
import qualified Luna.Syntax.Term.Lit  as Lit

import qualified Data.Construction as C


-------------------
-- === Utils === --
-------------------

type DynName d a = NameByDynamics d a
type NameByDynamics dyn d = ByDynamics dyn Lit.String d


-------------------
-- === Exprs === --
-------------------

-- === Definitions === --

class Expression t where exprArgs :: Iso (Expr t d a) (Expr t d' a') (Args (Expr t d a)) (Args (Expr t d' a'))

data family Expr  t  dyn a
type        Exprs ts dyn a = Expr <$> ts <*> '[dyn] <*> '[a]

newtype instance Expr Atom.Var'    dyn a = Var     (DynName dyn a)
newtype instance Expr Atom.Cons'   dyn a = Cons    (DynName dyn a)
data    instance Expr Atom.Acc'    dyn a = Acc    !(DynName dyn a) !a
data    instance Expr Atom.App'    dyn a = App                     !a ![Arg a]
data    instance Expr Atom.Curry'  dyn a = Curry                   !a ![Arg a]
data    instance Expr Atom.Unify'  dyn a = Unify                   !a !a
data    instance Expr Atom.Match'  dyn a = Match                   !a !a
data    instance Expr Atom.Lam'    dyn a = Lam                     ![Arg a] !a
data    instance Expr Atom.Native' dyn a = Native !(DynName dyn a)
data    instance Expr Atom.Blank'  dyn a = Blank


-- === Instances === --

type instance Base (Expr t dyn a) = t

type instance Args (Expr Atom.Var'    dyn a) = OneTuple (DynName dyn a)
type instance Args (Expr Atom.Cons'   dyn a) = OneTuple (DynName dyn a)
type instance Args (Expr Atom.Acc'    dyn a) =          (DynName dyn a, a)
type instance Args (Expr Atom.App'    dyn a) =          (a, [Arg a])
type instance Args (Expr Atom.Curry'  dyn a) =          (a, [Arg a])
type instance Args (Expr Atom.Unify'  dyn a) =          (a, a)
type instance Args (Expr Atom.Match'  dyn a) =          (a, a)
type instance Args (Expr Atom.Lam'    dyn a) =          ([Arg a], a)
type instance Args (Expr Atom.Native' dyn a) = OneTuple (DynName dyn a)
type instance Args (Expr Atom.Blank'  dyn a) =          ()

instance Expression Atom.Var'    where exprArgs = iso (\(Var    t1   ) -> OneTuple t1) (uncurry Var   ) ; {-# INLINE exprArgs #-}
instance Expression Atom.Cons'   where exprArgs = iso (\(Cons   t1   ) -> OneTuple t1) (uncurry Cons  ) ; {-# INLINE exprArgs #-}
instance Expression Atom.Acc'    where exprArgs = iso (\(Acc    t1 t2) -> (t1,t2)    ) (uncurry Acc   ) ; {-# INLINE exprArgs #-}
instance Expression Atom.App'    where exprArgs = iso (\(App    t1 t2) -> (t1,t2)    ) (uncurry App   ) ; {-# INLINE exprArgs #-}
instance Expression Atom.Curry'  where exprArgs = iso (\(Curry  t1 t2) -> (t1,t2)    ) (uncurry Curry ) ; {-# INLINE exprArgs #-}
instance Expression Atom.Unify'  where exprArgs = iso (\(Unify  t1 t2) -> (t1,t2)    ) (uncurry Unify ) ; {-# INLINE exprArgs #-}
instance Expression Atom.Match'  where exprArgs = iso (\(Match  t1 t2) -> (t1,t2)    ) (uncurry Match ) ; {-# INLINE exprArgs #-}
instance Expression Atom.Lam'    where exprArgs = iso (\(Lam    t1 t2) -> (t1,t2)    ) (uncurry Lam   ) ; {-# INLINE exprArgs #-}
instance Expression Atom.Native' where exprArgs = iso (\(Native t1   ) -> OneTuple t1) (uncurry Native) ; {-# INLINE exprArgs #-}
instance Expression Atom.Blank'  where exprArgs = iso (\ Blank         -> ()         ) (uncurry Blank ) ; {-# INLINE exprArgs #-}

instance Expression t => DataType (Expr t dyn a) (Expr t dyn' a') where args = exprArgs ; {-# INLINE args #-}
