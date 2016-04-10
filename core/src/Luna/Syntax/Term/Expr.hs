module Luna.Syntax.Term.Expr (module Luna.Syntax.Term.Expr, module X) where

import Prelude.Luna
import Luna.Syntax.Term.Atom as X (Var', Cons', Acc', App', Curry', Unify', Match', Lam', Native', Blank') -- Types only

import Data.Base                 (Base)
import Data.Construction         (Args, DataType (args))
import Luna.Runtime.Dynamics     (ByDynamics)
import Luna.Syntax.Term.Function (Arg)
import Type.Applicative

import qualified Luna.Syntax.Term.Atom_OLD as Atom
import qualified Luna.Syntax.Term.Lit  as Lit

import qualified Data.Construction as C


-- TODO[WD]: move to issue tracker after releasing Luna to github
--------------------------------------------
-- === Enhancement proposals & issues === --
--------------------------------------------

-- Status: pending | accepted | rejected

-- Reporter  Status   Description
-- wdanilo   pending  ACCESSORS AND FUNCTIONS UNIFICATION
--                    Check if we can throw away accessors in terms. Let's consider the following Luna code:
--                        a  = x.bar
--                        a' = acc x "bar"
--                    These lines should mean exactly the same with the followings rules:
--                        - both forms have to be distinguishable to provide Term <-> Text conversion
--                        - the performance of STATIC Luna compilation should be as fast as in current solution
--                        - accessors should be first class objects, althought we can easily make a workaround like `myacc = a : a.x`


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

newtype instance Expr Var'    dyn a = Var     (DynName dyn a)
newtype instance Expr Cons'   dyn a = Cons    (DynName dyn a)
data    instance Expr Acc'    dyn a = Acc    !(DynName dyn a) !a
data    instance Expr App'    dyn a = App                     !a ![Arg a]
data    instance Expr Curry'  dyn a = Curry                   !a ![Arg a]
data    instance Expr Unify'  dyn a = Unify                   !a !a
data    instance Expr Match'  dyn a = Match                   !a !a
data    instance Expr Lam'    dyn a = Lam                     ![Arg a] !a
data    instance Expr Native' dyn a = Native !(DynName dyn a)
data    instance Expr Blank'  dyn a = Blank


-- === Instances === --

type instance Base (Expr t dyn a) = t

type instance Args (Expr Var'    dyn a) = OneTuple (DynName dyn a)
type instance Args (Expr Cons'   dyn a) = OneTuple (DynName dyn a)
type instance Args (Expr Acc'    dyn a) =          (DynName dyn a, a)
type instance Args (Expr App'    dyn a) =          (a, [Arg a])
type instance Args (Expr Curry'  dyn a) =          (a, [Arg a])
type instance Args (Expr Unify'  dyn a) =          (a, a)
type instance Args (Expr Match'  dyn a) =          (a, a)
type instance Args (Expr Lam'    dyn a) =          ([Arg a], a)
type instance Args (Expr Native' dyn a) = OneTuple (DynName dyn a)
type instance Args (Expr Blank'  dyn a) =          ()

instance Expression Var'    where exprArgs = iso (\(Var    t1   ) -> OneTuple t1) (uncurry Var   ) ; {-# INLINE exprArgs #-}
instance Expression Cons'   where exprArgs = iso (\(Cons   t1   ) -> OneTuple t1) (uncurry Cons  ) ; {-# INLINE exprArgs #-}
instance Expression Acc'    where exprArgs = iso (\(Acc    t1 t2) -> (t1,t2)    ) (uncurry Acc   ) ; {-# INLINE exprArgs #-}
instance Expression App'    where exprArgs = iso (\(App    t1 t2) -> (t1,t2)    ) (uncurry App   ) ; {-# INLINE exprArgs #-}
instance Expression Curry'  where exprArgs = iso (\(Curry  t1 t2) -> (t1,t2)    ) (uncurry Curry ) ; {-# INLINE exprArgs #-}
instance Expression Unify'  where exprArgs = iso (\(Unify  t1 t2) -> (t1,t2)    ) (uncurry Unify ) ; {-# INLINE exprArgs #-}
instance Expression Match'  where exprArgs = iso (\(Match  t1 t2) -> (t1,t2)    ) (uncurry Match ) ; {-# INLINE exprArgs #-}
instance Expression Lam'    where exprArgs = iso (\(Lam    t1 t2) -> (t1,t2)    ) (uncurry Lam   ) ; {-# INLINE exprArgs #-}
instance Expression Native' where exprArgs = iso (\(Native t1   ) -> OneTuple t1) (uncurry Native) ; {-# INLINE exprArgs #-}
instance Expression Blank'  where exprArgs = iso (\ Blank         -> ()         ) (uncurry Blank ) ; {-# INLINE exprArgs #-}

instance Expression t => DataType (Expr t dyn a) (Expr t dyn' a') where args = exprArgs ; {-# INLINE args #-}
