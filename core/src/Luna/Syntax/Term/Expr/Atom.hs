module Luna.Syntax.Term.Expr.Atom (module Luna.Syntax.Term.Expr.Atom, module X) where

import qualified Prelude.Luna as P
import           Prelude.Luna hiding (String, Integer, Rational, Curry)

import Luna.Syntax.Term.Expr.Symbol as X (String, Integer, Rational, Acc, App, Blank, Cons, Curry, Lam, Match, Missing, Native, Star, Unify, Var) -- Types only

import Data.Base                 (Base)
import Data.Construction         (Args, DataType (args))
import Luna.Runtime.Dynamics     (ByDynamics)
import Luna.Syntax.Term.Function (Arg)
import Type.Applicative

import qualified Old.Luna.Syntax.Term.Expr.Lit  as Lit

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
-- === Atoms === --
-------------------

-- === Abstractions === --

class Atomic t where atomArgs :: Iso (Atom t d a) (Atom t d' a') (Args (Atom t d a)) (Args (Atom t d' a'))

data family Atom  t  dyn a
type        Atoms ts dyn a = Atom <$> ts <*> '[dyn] <*> '[a]


-- === Definitions === --

newtype instance Atom Integer  dyn a = Integer  P.Integer
newtype instance Atom Rational dyn a = Rational P.Rational
newtype instance Atom String   dyn a = String   P.String

data    instance Atom Acc      dyn a = Acc    !(DynName dyn a) !a
data    instance Atom App      dyn a = App                     !a ![Arg a]
data    instance Atom Blank    dyn a = Blank
data    instance Atom Curry    dyn a = Curry                   !a ![Arg a]
data    instance Atom Lam      dyn a = Lam                     ![Arg a] !a
data    instance Atom Match    dyn a = Match                   !a !a
data    instance Atom Missing  dyn a = Missing
data    instance Atom Native   dyn a = Native !(DynName dyn a)
data    instance Atom Star     dyn a = Star
data    instance Atom Unify    dyn a = Unify                   !a !a
newtype instance Atom Cons     dyn a = Cons    (DynName dyn a)
newtype instance Atom Var      dyn a = Var     (DynName dyn a)


-- === Instances === --

type instance Base (Atom t dyn a) = t

-- Args

type instance Args (Atom Integer  dyn a) = OneTuple P.Integer
type instance Args (Atom Rational dyn a) = OneTuple P.Rational
type instance Args (Atom String   dyn a) = OneTuple P.String

type instance Args (Atom Acc     dyn a) =          (DynName dyn a, a)
type instance Args (Atom App     dyn a) =          (a, [Arg a])
type instance Args (Atom Blank   dyn a) =          ()
type instance Args (Atom Cons    dyn a) = OneTuple (DynName dyn a)
type instance Args (Atom Curry   dyn a) =          (a, [Arg a])
type instance Args (Atom Lam     dyn a) =          ([Arg a], a)
type instance Args (Atom Match   dyn a) =          (a, a)
type instance Args (Atom Missing dyn a) =          ()
type instance Args (Atom Native  dyn a) = OneTuple (DynName dyn a)
type instance Args (Atom Star    dyn a) =          ()
type instance Args (Atom Unify   dyn a) =          (a, a)
type instance Args (Atom Var     dyn a) = OneTuple (DynName dyn a)

-- Atomic

instance Atomic Integer  where atomArgs = iso (\(Integer  t1) -> OneTuple t1) (uncurry Integer  ) ; {-# INLINE atomArgs #-}
instance Atomic Rational where atomArgs = iso (\(Rational t1) -> OneTuple t1) (uncurry Rational ) ; {-# INLINE atomArgs #-}
instance Atomic String   where atomArgs = iso (\(String   t1) -> OneTuple t1) (uncurry String   ) ; {-# INLINE atomArgs #-}

instance Atomic Acc     where atomArgs = iso (\(Acc    t1 t2) -> (t1,t2)    ) (uncurry Acc    ) ; {-# INLINE atomArgs #-}
instance Atomic App     where atomArgs = iso (\(App    t1 t2) -> (t1,t2)    ) (uncurry App    ) ; {-# INLINE atomArgs #-}
instance Atomic Blank   where atomArgs = iso (\ Blank         -> ()         ) (uncurry Blank  ) ; {-# INLINE atomArgs #-}
instance Atomic Cons    where atomArgs = iso (\(Cons   t1   ) -> OneTuple t1) (uncurry Cons   ) ; {-# INLINE atomArgs #-}
instance Atomic Curry   where atomArgs = iso (\(Curry  t1 t2) -> (t1,t2)    ) (uncurry Curry  ) ; {-# INLINE atomArgs #-}
instance Atomic Lam     where atomArgs = iso (\(Lam    t1 t2) -> (t1,t2)    ) (uncurry Lam    ) ; {-# INLINE atomArgs #-}
instance Atomic Match   where atomArgs = iso (\(Match  t1 t2) -> (t1,t2)    ) (uncurry Match  ) ; {-# INLINE atomArgs #-}
instance Atomic Missing where atomArgs = iso (\ Missing       -> ()         ) (uncurry Missing) ; {-# INLINE atomArgs #-}
instance Atomic Native  where atomArgs = iso (\(Native t1   ) -> OneTuple t1) (uncurry Native ) ; {-# INLINE atomArgs #-}
instance Atomic Star    where atomArgs = iso (\ Star          -> ()         ) (uncurry Star   ) ; {-# INLINE atomArgs #-}
instance Atomic Unify   where atomArgs = iso (\(Unify  t1 t2) -> (t1,t2)    ) (uncurry Unify  ) ; {-# INLINE atomArgs #-}
instance Atomic Var     where atomArgs = iso (\(Var    t1   ) -> OneTuple t1) (uncurry Var    ) ; {-# INLINE atomArgs #-}

-- DataType

instance Atomic t => DataType (Atom t dyn a) (Atom t dyn' a') where args = atomArgs ; {-# INLINE args #-}
