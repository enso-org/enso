{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Term.Expr.Symbol.Named (module Luna.Syntax.Term.Expr.Symbol.Named, module X) where

import qualified Prelude.Luna as P
import           Prelude.Luna hiding (Symbol, String, Integer, Rational, Curry, Data)

import Luna.Syntax.Term.Expr.Symbol as X (Symbol)
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

import Data.Construction
import           Data.RTuple (List(Null, (:-:)))
import qualified Data.RTuple as List



---------------------
-- === Symbols === --
---------------------

-- === Definitions === --

type NamedSymbol t n a = Symbol t (Layout.Named n a)


newtype instance Symbol Integer  (Layout.Named n a) = Integer  P.Integer
newtype instance Symbol Rational (Layout.Named n a) = Rational P.Rational
newtype instance Symbol String   (Layout.Named n a) = String   P.String

data    instance Symbol Acc      (Layout.Named n a) = Acc     !n !a
data    instance Symbol App      (Layout.Named n a) = App     !a ![Arg a]
data    instance Symbol Blank    (Layout.Named n a) = Blank
newtype instance Symbol Cons     (Layout.Named n a) = Cons     n
data    instance Symbol Curry    (Layout.Named n a) = Curry   !a ![Arg a]
data    instance Symbol Lam      (Layout.Named n a) = Lam     ![Arg a] !a
data    instance Symbol Match    (Layout.Named n a) = Match   !a !a
data    instance Symbol Missing  (Layout.Named n a) = Missing
data    instance Symbol Native   (Layout.Named n a) = Native  !n
data    instance Symbol Star     (Layout.Named n a) = Star
data    instance Symbol Unify    (Layout.Named n a) = Unify   !a !a
newtype instance Symbol Var      (Layout.Named n a) = Var      n



-- === Instances === --

-- Show

deriving instance ShowFields (NamedSymbol Acc     n a) => Show (NamedSymbol Acc     n a)
deriving instance ShowFields (NamedSymbol App     n a) => Show (NamedSymbol App     n a)
deriving instance ShowFields (NamedSymbol Blank   n a) => Show (NamedSymbol Blank   n a)
deriving instance ShowFields (NamedSymbol Cons    n a) => Show (NamedSymbol Cons    n a)
deriving instance ShowFields (NamedSymbol Curry   n a) => Show (NamedSymbol Curry   n a)
deriving instance ShowFields (NamedSymbol Lam     n a) => Show (NamedSymbol Lam     n a)
deriving instance ShowFields (NamedSymbol Match   n a) => Show (NamedSymbol Match   n a)
deriving instance ShowFields (NamedSymbol Missing n a) => Show (NamedSymbol Missing n a)
deriving instance ShowFields (NamedSymbol Native  n a) => Show (NamedSymbol Native  n a)
deriving instance ShowFields (NamedSymbol Star    n a) => Show (NamedSymbol Star    n a)
deriving instance ShowFields (NamedSymbol Unify   n a) => Show (NamedSymbol Unify   n a)
deriving instance ShowFields (NamedSymbol Var     n a) => Show (NamedSymbol Var     n a)

-- Args

type instance Fields (NamedSymbol Integer  n a) = '[P.Integer]
type instance Fields (NamedSymbol Rational n a) = '[P.Rational]
type instance Fields (NamedSymbol String   n a) = '[P.String]

type instance Fields (NamedSymbol Acc      n a) = '[n, a]
type instance Fields (NamedSymbol App      n a) = '[a, [Arg a]]
type instance Fields (NamedSymbol Blank    n a) = '[]
type instance Fields (NamedSymbol Cons     n a) = '[n]
type instance Fields (NamedSymbol Curry    n a) = '[a, [Arg a]]
type instance Fields (NamedSymbol Lam      n a) = '[[Arg a], a]
type instance Fields (NamedSymbol Match    n a) = '[a, a]
type instance Fields (NamedSymbol Missing  n a) = '[]
type instance Fields (NamedSymbol Native   n a) = '[n]
type instance Fields (NamedSymbol Star     n a) = '[]
type instance Fields (NamedSymbol Unify    n a) = '[a, a]
type instance Fields (NamedSymbol Var      n a) = '[n]

-- Products

instance Product (NamedSymbol Integer  n a) (NamedSymbol Integer  n' a') where fields = iso (\(Integer  t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Integer  t1    ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Rational n a) (NamedSymbol Rational n' a') where fields = iso (\(Rational t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Rational t1    ) ; {-# INLINE fields #-}
instance Product (NamedSymbol String   n a) (NamedSymbol String   n' a') where fields = iso (\(String   t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> String   t1    ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Acc      n a) (NamedSymbol Acc      n' a') where fields = iso (\(Acc      t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> Acc      t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedSymbol App      n a) (NamedSymbol App      n' a') where fields = iso (\(App      t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> App      t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Blank    n a) (NamedSymbol Blank    n' a') where fields = iso (\ Blank           -> Null              ) (\(Null              ) -> Blank          ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Cons     n a) (NamedSymbol Cons     n' a') where fields = iso (\(Cons     t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Cons     t1    ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Curry    n a) (NamedSymbol Curry    n' a') where fields = iso (\(Curry    t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> Curry    t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Lam      n a) (NamedSymbol Lam      n' a') where fields = iso (\(Lam      t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> Lam      t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Match    n a) (NamedSymbol Match    n' a') where fields = iso (\(Match    t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> Match    t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Missing  n a) (NamedSymbol Missing  n' a') where fields = iso (\ Missing         -> Null              ) (\(Null              ) -> Missing        ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Native   n a) (NamedSymbol Native   n' a') where fields = iso (\(Native   t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Native   t1    ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Star     n a) (NamedSymbol Star     n' a') where fields = iso (\ Star            -> Null              ) (\(Null              ) -> Star           ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Unify    n a) (NamedSymbol Unify    n' a') where fields = iso (\(Unify    t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> Unify    t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Var      n a) (NamedSymbol Var      n' a') where fields = iso (\(Var      t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Var      t1    ) ; {-# INLINE fields #-}


--------------------------
-- === Construction === --
--------------------------

symbol :: (Product s s, List.Curry' f, List.Uncurried' f ~ (List (Fields s) -> s)) => f
symbol = List.curry' $ view (from fields) ; {-# INLINE symbol #-}


integer :: P.Integer -> NamedSymbol Integer n a
integer = symbol ; {-# INLINE integer #-}

rational :: P.Rational -> NamedSymbol Rational n a
rational = symbol ; {-# INLINE rational #-}

string :: P.String -> NamedSymbol String n a
string = symbol ; {-# INLINE string #-}


acc :: n -> a -> NamedSymbol Acc  n a
acc = symbol ; {-# INLINE acc #-}

app :: a -> [Arg a] -> NamedSymbol App n a
app = symbol ; {-# INLINE app #-}

blank :: NamedSymbol Blank n a
blank = symbol ; {-# INLINE blank #-}

cons :: n -> NamedSymbol Cons n a
cons = symbol ; {-# INLINE cons #-}

curry :: a -> [Arg a] -> NamedSymbol Curry n a
curry = symbol ; {-# INLINE curry #-}

lam :: [Arg a] -> a -> NamedSymbol Lam n a
lam = symbol ; {-# INLINE lam #-}

match :: a -> a -> NamedSymbol Match n a
match = symbol ; {-# INLINE match #-}

missing :: NamedSymbol Missing n a
missing = symbol ; {-# INLINE missing #-}

star :: NamedSymbol Star n a
star = symbol ; {-# INLINE star #-}

unify :: a -> a -> NamedSymbol Unify n a
unify = symbol ; {-# INLINE unify #-}

var :: n -> NamedSymbol Var n a
var = symbol ; {-# INLINE var #-}
