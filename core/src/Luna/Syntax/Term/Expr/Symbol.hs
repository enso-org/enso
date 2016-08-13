{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Term.Expr.Symbol (module Luna.Syntax.Term.Expr.Symbol, module X) where

import qualified Prelude.Luna as P
import           Prelude.Luna hiding (Symbol, String, Integer, Rational, Curry, Data)

import Luna.Syntax.Term.Expr.Atom as X (Atom, String, Integer, Rational, Acc, App, Blank, Cons, Curry, Lam, Match, Missing, Native, Star, Unify, Var) -- Types only

import Data.Base                 (Base)
import Data.Construction         (Args)
import Luna.Runtime.Dynamics     (Dynamics, ByDynamics)
import Luna.Syntax.Term.Function (Arg)
import Type.Applicative
import Control.Lens.Property
import Data.Phantom
import Luna.Syntax.Term.Expr.Format
import qualified Luna.Syntax.Term.Expr.Layout as Layout
import           Luna.Syntax.Term.Expr.Layout (Layout)

import qualified Old.Luna.Syntax.Term.Expr.Lit  as Lit

import Data.Construction
import           Data.RTuple (List(Null, (:-:)))
import qualified Data.RTuple as List



---------------------
-- === Symbols === --
---------------------

-- === Definitions === --

data family Symbol2  atom  layout
type        Symbols2 atoms layout = Symbol2 <$> atoms <*> '[layout]


-- === Selectors === --

-- data Sym = Sym deriving (Show)

type instance Get    Sym (Symbol2 atom layout) = Symbol2 atom layout
instance      Getter Sym (Symbol2 atom layout) where
    get = id ; {-# INLINE get #-}


-- === Instances === --

-- Properties

type instance Get Atom          (Symbol2 atom _     ) = atom
type instance Set Atom   atom   (Symbol2 _    layout) = (Symbol2 atom layout)

type instance Get Layout        (Symbol2 _    layout) = layout
type instance Set Layout layout (Symbol2 atom _     ) = (Symbol2 atom layout)

type instance Get Format        (Symbol2 atom _     ) = Get Format atom

instance Phantom atom => Getter Atom     (Symbol2 atom layout) where get _ = phantom



--
-- ---------------------------------------------------------
-- -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! --
-- ---------------------------------------------------------
-- -- DEPRECIATED
--
--
--
--
--
--
-- -- TODO[WD]: move to issue tracker after releasing Luna to github
-- --------------------------------------------
-- -- === Enhancement proposals & issues === --
-- --------------------------------------------
--
-- -- Status: pending | accepted | rejected
--
-- -- Reporter  Status   Description
-- -- wdanilo   pending  ACCESSORS AND FUNCTIONS UNIFICATION
-- --                    Check if we can throw away accessors in terms. Let's consider the following Luna code:
-- --                        a  = x.bar
-- --                        a' = acc x "bar"
-- --                    These lines should mean exactly the same with the followings rules:
-- --                        - both forms have to be distinguishable to provide Term <-> Text conversion
-- --                        - the performance of STATIC Luna compilation should be as fast as in current solution
-- --                        - accessors should be first class objects, althought we can easily make a workaround like `myacc = a : a.x`
--
--
-- ---------------------
-- -- === Symbols === --
-- ---------------------
--
-- -- === Abstractions === --
--
-- class IsSymbol t where symbolArgs :: Iso (Symbol t d a) (Symbol t d' a') (Args (Symbol t d a)) (Args (Symbol t d' a'))
--
-- data family Symbol  atom  dyn a
-- type        Symbols atoms dyn a = Symbol <$> atoms <*> '[dyn] <*> '[a]
--
--
-- -- === Selectors === --
--
data Sym = Sym deriving (Show)
--
-- type instance Get    Sym (Symbol atom dyn a) = Symbol atom dyn a
-- instance      Getter Sym (Symbol atom dyn a) where
--     get = id ; {-# INLINE get #-}
--
-- -- === Utils === --
--
-- type DynName d a = NameByDynamics d a
type NameByDynamics dyn d = ByDynamics dyn Lit.String d
--
--
-- -- === Definitions === --
--
-- newtype instance Symbol Integer  dyn a = Integer  P.Integer
-- newtype instance Symbol Rational dyn a = Rational P.Rational
-- newtype instance Symbol String   dyn a = String   P.String
--
-- data    instance Symbol Acc      dyn a = Acc    !(DynName dyn a) !a
-- data    instance Symbol App      dyn a = App                     !a ![Arg a]
-- data    instance Symbol Blank    dyn a = Blank
-- newtype instance Symbol Cons     dyn a = Cons    (DynName dyn a)
-- data    instance Symbol Curry    dyn a = Curry                   !a ![Arg a]
-- data    instance Symbol Lam      dyn a = Lam                     ![Arg a] !a
-- data    instance Symbol Match    dyn a = Match                   !a !a
-- data    instance Symbol Missing  dyn a = Missing
-- data    instance Symbol Native   dyn a = Native !(DynName dyn a)
-- data    instance Symbol Star     dyn a = Star
-- data    instance Symbol Unify    dyn a = Unify                   !a !a
-- newtype instance Symbol Var      dyn a = Var     (DynName dyn a)
--
--
--
-- -- === Instances === --
--
-- -- Properties
--
-- data Binding = Binding deriving (Show)
--
-- type instance Get Atom          (Symbol atom _   _) = atom
-- type instance Set Atom     atom (Symbol _    dyn a) = (Symbol atom dyn a)
--
-- type instance Get Dynamics      (Symbol _    dyn _) = dyn
-- type instance Set Dynamics dyn  (Symbol atom dyn a) = (Symbol atom dyn a)
--
-- type instance Get Binding       (Symbol _    _   a) = a
-- type instance Set Binding  a    (Symbol atom dyn _) = (Symbol atom dyn a)
--
-- type instance Get Format        (Symbol atom _   _) = Get Format atom
--
-- instance Phantom atom => Getter Atom     (Symbol atom dyn a) where get _ = phantom
-- instance Phantom dyn  => Getter Dynamics (Symbol atom dyn a) where get _ = phantom
--
--
-- -- Show
--
-- deriving instance (Show a, Show (DynName dyn a)) => Show (Symbol Acc     dyn a)
-- deriving instance  Show a                        => Show (Symbol App     dyn a)
-- deriving instance                                   Show (Symbol Blank   dyn a)
-- deriving instance          Show (DynName dyn a)  => Show (Symbol Cons    dyn a)
-- deriving instance  Show a                        => Show (Symbol Curry   dyn a)
-- deriving instance  Show a                        => Show (Symbol Lam     dyn a)
-- deriving instance  Show a                        => Show (Symbol Match   dyn a)
-- deriving instance                                   Show (Symbol Missing dyn a)
-- deriving instance          Show (DynName dyn a)  => Show (Symbol Native  dyn a)
-- deriving instance                                   Show (Symbol Star    dyn a)
-- deriving instance  Show a                        => Show (Symbol Unify   dyn a)
-- deriving instance          Show (DynName dyn a)  => Show (Symbol Var     dyn a)
--
-- -- Args
--
-- type instance Args (Symbol Integer  dyn a) = OneTuple P.Integer
-- type instance Args (Symbol Rational dyn a) = OneTuple P.Rational
-- type instance Args (Symbol String   dyn a) = OneTuple P.String
--
-- type instance Args (Symbol Acc     dyn a) =          (DynName dyn a, a)
-- type instance Args (Symbol App     dyn a) =          (a, [Arg a])
-- type instance Args (Symbol Blank   dyn a) =          ()
-- type instance Args (Symbol Cons    dyn a) = OneTuple (DynName dyn a)
-- type instance Args (Symbol Curry   dyn a) =          (a, [Arg a])
-- type instance Args (Symbol Lam     dyn a) =          ([Arg a], a)
-- type instance Args (Symbol Match   dyn a) =          (a, a)
-- type instance Args (Symbol Missing dyn a) =          ()
-- type instance Args (Symbol Native  dyn a) = OneTuple (DynName dyn a)
-- type instance Args (Symbol Star    dyn a) =          ()
-- type instance Args (Symbol Unify   dyn a) =          (a, a)
-- type instance Args (Symbol Var     dyn a) = OneTuple (DynName dyn a)
--
-- -- IsSymbol
--
-- instance IsSymbol Integer  where symbolArgs = iso (\(Integer  t1) -> OneTuple t1) (uncurry Integer  ) ; {-# INLINE symbolArgs #-}
-- instance IsSymbol Rational where symbolArgs = iso (\(Rational t1) -> OneTuple t1) (uncurry Rational ) ; {-# INLINE symbolArgs #-}
-- instance IsSymbol String   where symbolArgs = iso (\(String   t1) -> OneTuple t1) (uncurry String   ) ; {-# INLINE symbolArgs #-}
--
-- instance IsSymbol Acc     where symbolArgs = iso (\(Acc    t1 t2) -> (t1,t2)    ) (uncurry Acc    ) ; {-# INLINE symbolArgs #-}
-- instance IsSymbol App     where symbolArgs = iso (\(App    t1 t2) -> (t1,t2)    ) (uncurry App    ) ; {-# INLINE symbolArgs #-}
-- instance IsSymbol Blank   where symbolArgs = iso (\ Blank         -> ()         ) (uncurry Blank  ) ; {-# INLINE symbolArgs #-}
-- instance IsSymbol Cons    where symbolArgs = iso (\(Cons   t1   ) -> OneTuple t1) (uncurry Cons   ) ; {-# INLINE symbolArgs #-}
-- instance IsSymbol Curry   where symbolArgs = iso (\(Curry  t1 t2) -> (t1,t2)    ) (uncurry Curry  ) ; {-# INLINE symbolArgs #-}
-- instance IsSymbol Lam     where symbolArgs = iso (\(Lam    t1 t2) -> (t1,t2)    ) (uncurry Lam    ) ; {-# INLINE symbolArgs #-}
-- instance IsSymbol Match   where symbolArgs = iso (\(Match  t1 t2) -> (t1,t2)    ) (uncurry Match  ) ; {-# INLINE symbolArgs #-}
-- instance IsSymbol Missing where symbolArgs = iso (\ Missing       -> ()         ) (uncurry Missing) ; {-# INLINE symbolArgs #-}
-- instance IsSymbol Native  where symbolArgs = iso (\(Native t1   ) -> OneTuple t1) (uncurry Native ) ; {-# INLINE symbolArgs #-}
-- instance IsSymbol Star    where symbolArgs = iso (\ Star          -> ()         ) (uncurry Star   ) ; {-# INLINE symbolArgs #-}
-- instance IsSymbol Unify   where symbolArgs = iso (\(Unify  t1 t2) -> (t1,t2)    ) (uncurry Unify  ) ; {-# INLINE symbolArgs #-}
-- instance IsSymbol Var     where symbolArgs = iso (\(Var    t1   ) -> OneTuple t1) (uncurry Var    ) ; {-# INLINE symbolArgs #-}
--
--
-- --------------------------
-- -- === Construction === --
-- --------------------------
--
-- symbol :: (IsSymbol t, Curry' f, s ~ Symbol t d a, Uncurried' f ~ (Args s -> s)) => f
-- symbol = curry' $ view (from symbolArgs) ; {-# INLINE symbol #-}
--
--
-- integer :: P.Integer -> Symbol Integer dyn a
-- integer = symbol ; {-# INLINE integer #-}
--
-- rational :: P.Rational -> Symbol Rational dyn a
-- rational = symbol ; {-# INLINE rational #-}
--
-- string :: P.String -> Symbol String dyn a
-- string = symbol ; {-# INLINE string #-}
--
--
-- acc :: DynName dyn a -> a -> Symbol Acc  dyn a
-- acc = symbol ; {-# INLINE acc #-}
--
-- app :: a -> [Arg a] -> Symbol App dyn a
-- app = symbol ; {-# INLINE app #-}
--
-- blank :: Symbol Blank dyn a
-- blank = symbol ; {-# INLINE blank #-}
--
-- cons :: DynName dyn a -> Symbol Cons dyn a
-- cons = symbol ; {-# INLINE cons #-}
--
-- curry :: a -> [Arg a] -> Symbol Curry dyn a
-- curry = symbol ; {-# INLINE curry #-}
--
-- lam :: [Arg a] -> a -> Symbol Lam dyn a
-- lam = symbol ; {-# INLINE lam #-}
--
-- match :: a -> a -> Symbol Match dyn a
-- match = symbol ; {-# INLINE match #-}
--
-- missing :: Symbol Missing dyn a
-- missing = symbol ; {-# INLINE missing #-}
--
-- star :: Symbol Star dyn a
-- star = symbol ; {-# INLINE star #-}
--
-- unify :: a -> a -> Symbol Unify dyn a
-- unify = symbol ; {-# INLINE unify #-}
--
-- var :: DynName dyn a -> Symbol Var dyn a
-- var = symbol ; {-# INLINE var #-}
