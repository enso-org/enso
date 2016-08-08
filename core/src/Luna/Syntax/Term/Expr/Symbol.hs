{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Term.Expr.Symbol (module Luna.Syntax.Term.Expr.Symbol, module X) where

import qualified Prelude.Luna as P
import           Prelude.Luna hiding (Symbol, String, Integer, Rational, Curry, Data)

import Luna.Syntax.Term.Expr.Atom as X (Atom, String, Integer, Rational, Acc, App, Blank, Cons, Curry, Lam, Match, Missing, Native, Star, Unify, Var) -- Types only

import Data.Base                 (Base)
import Data.Construction         (Args, DataType (args))
import Luna.Runtime.Dynamics     (Dynamics, ByDynamics)
import Luna.Syntax.Term.Function (Arg)
import Type.Applicative
import Control.Lens.Property
import Data.Phantom
import Luna.Syntax.Term.Expr.Format

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
-- === Datas === --
-------------------

-- === Abstractions === --

class IsSymbol t where symbolArgs :: Iso (Data t d a) (Data t d' a') (Args (Data t d a)) (Args (Data t d' a'))

data family Data  atom  dyn a
type        Datas atoms dyn a = Data <$> atoms <*> '[dyn] <*> '[a]


-- === Selectors === --

data Symbol = Symbol deriving (Show)

type instance Get    Symbol (Data atom dyn a) = Data atom dyn a
instance      Getter Symbol (Data atom dyn a) where
    get = id ; {-# INLINE get #-}


-- === Definitions === --

newtype instance Data Integer  dyn a = Integer  P.Integer
newtype instance Data Rational dyn a = Rational P.Rational
newtype instance Data String   dyn a = String   P.String

data    instance Data Acc      dyn a = Acc    !(DynName dyn a) !a
data    instance Data App      dyn a = App                     !a ![Arg a]
data    instance Data Blank    dyn a = Blank
newtype instance Data Cons     dyn a = Cons    (DynName dyn a)
data    instance Data Curry    dyn a = Curry                   !a ![Arg a]
data    instance Data Lam      dyn a = Lam                     ![Arg a] !a
data    instance Data Match    dyn a = Match                   !a !a
data    instance Data Missing  dyn a = Missing
data    instance Data Native   dyn a = Native !(DynName dyn a)
data    instance Data Star     dyn a = Star
data    instance Data Unify    dyn a = Unify                   !a !a
newtype instance Data Var      dyn a = Var     (DynName dyn a)



-- === Instances === --

-- Properties

data Binding = Binding deriving (Show)

type instance Get Atom          (Data atom _   _) = atom
type instance Set Atom     atom (Data _    dyn a) = (Data atom dyn a)

type instance Get Dynamics      (Data _    dyn _) = dyn
type instance Set Dynamics dyn  (Data atom dyn a) = (Data atom dyn a)

type instance Get Binding       (Data _    _   a) = a
type instance Set Binding  a    (Data atom dyn _) = (Data atom dyn a)

instance Phantom atom => Getter Atom     (Data atom dyn a) where get _ = phantom
instance Phantom dyn  => Getter Dynamics (Data atom dyn a) where get _ = phantom

type instance Get Format (Data atom _ _) = Get Format atom

-- Show

deriving instance (Show a, Show (DynName dyn a)) => Show (Data Acc     dyn a)
deriving instance  Show a                        => Show (Data App     dyn a)
deriving instance                                   Show (Data Blank   dyn a)
deriving instance          Show (DynName dyn a)  => Show (Data Cons    dyn a)
deriving instance  Show a                        => Show (Data Curry   dyn a)
deriving instance  Show a                        => Show (Data Lam     dyn a)
deriving instance  Show a                        => Show (Data Match   dyn a)
deriving instance                                   Show (Data Missing dyn a)
deriving instance          Show (DynName dyn a)  => Show (Data Native  dyn a)
deriving instance                                   Show (Data Star    dyn a)
deriving instance  Show a                        => Show (Data Unify   dyn a)
deriving instance          Show (DynName dyn a)  => Show (Data Var     dyn a)


-- Args

type instance Args (Data Integer  dyn a) = OneTuple P.Integer
type instance Args (Data Rational dyn a) = OneTuple P.Rational
type instance Args (Data String   dyn a) = OneTuple P.String

type instance Args (Data Acc     dyn a) =          (DynName dyn a, a)
type instance Args (Data App     dyn a) =          (a, [Arg a])
type instance Args (Data Blank   dyn a) =          ()
type instance Args (Data Cons    dyn a) = OneTuple (DynName dyn a)
type instance Args (Data Curry   dyn a) =          (a, [Arg a])
type instance Args (Data Lam     dyn a) =          ([Arg a], a)
type instance Args (Data Match   dyn a) =          (a, a)
type instance Args (Data Missing dyn a) =          ()
type instance Args (Data Native  dyn a) = OneTuple (DynName dyn a)
type instance Args (Data Star    dyn a) =          ()
type instance Args (Data Unify   dyn a) =          (a, a)
type instance Args (Data Var     dyn a) = OneTuple (DynName dyn a)

-- IsSymbol

instance IsSymbol Integer  where symbolArgs = iso (\(Integer  t1) -> OneTuple t1) (uncurry Integer  ) ; {-# INLINE symbolArgs #-}
instance IsSymbol Rational where symbolArgs = iso (\(Rational t1) -> OneTuple t1) (uncurry Rational ) ; {-# INLINE symbolArgs #-}
instance IsSymbol String   where symbolArgs = iso (\(String   t1) -> OneTuple t1) (uncurry String   ) ; {-# INLINE symbolArgs #-}

instance IsSymbol Acc     where symbolArgs = iso (\(Acc    t1 t2) -> (t1,t2)    ) (uncurry Acc    ) ; {-# INLINE symbolArgs #-}
instance IsSymbol App     where symbolArgs = iso (\(App    t1 t2) -> (t1,t2)    ) (uncurry App    ) ; {-# INLINE symbolArgs #-}
instance IsSymbol Blank   where symbolArgs = iso (\ Blank         -> ()         ) (uncurry Blank  ) ; {-# INLINE symbolArgs #-}
instance IsSymbol Cons    where symbolArgs = iso (\(Cons   t1   ) -> OneTuple t1) (uncurry Cons   ) ; {-# INLINE symbolArgs #-}
instance IsSymbol Curry   where symbolArgs = iso (\(Curry  t1 t2) -> (t1,t2)    ) (uncurry Curry  ) ; {-# INLINE symbolArgs #-}
instance IsSymbol Lam     where symbolArgs = iso (\(Lam    t1 t2) -> (t1,t2)    ) (uncurry Lam    ) ; {-# INLINE symbolArgs #-}
instance IsSymbol Match   where symbolArgs = iso (\(Match  t1 t2) -> (t1,t2)    ) (uncurry Match  ) ; {-# INLINE symbolArgs #-}
instance IsSymbol Missing where symbolArgs = iso (\ Missing       -> ()         ) (uncurry Missing) ; {-# INLINE symbolArgs #-}
instance IsSymbol Native  where symbolArgs = iso (\(Native t1   ) -> OneTuple t1) (uncurry Native ) ; {-# INLINE symbolArgs #-}
instance IsSymbol Star    where symbolArgs = iso (\ Star          -> ()         ) (uncurry Star   ) ; {-# INLINE symbolArgs #-}
instance IsSymbol Unify   where symbolArgs = iso (\(Unify  t1 t2) -> (t1,t2)    ) (uncurry Unify  ) ; {-# INLINE symbolArgs #-}
instance IsSymbol Var     where symbolArgs = iso (\(Var    t1   ) -> OneTuple t1) (uncurry Var    ) ; {-# INLINE symbolArgs #-}

-- DataType

instance IsSymbol t => DataType (Data t dyn a) (Data t dyn' a') where args = symbolArgs ; {-# INLINE args #-}
