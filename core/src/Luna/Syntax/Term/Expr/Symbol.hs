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

data family Symbol  atom  layout
type        Symbols atoms layout = Symbol <$> atoms <*> '[layout]


-- === Selectors === --

data Sym = Sym deriving (Show)

type instance Get    Sym (Symbol atom layout) = Symbol atom layout
instance      Getter Sym (Symbol atom layout) where
    get = id ; {-# INLINE get #-}


-- === Instances === --

-- Properties

type instance Get Atom          (Symbol atom _     ) = atom
type instance Set Atom   atom   (Symbol _    layout) = (Symbol atom layout)

type instance Get Layout        (Symbol _    layout) = layout
type instance Set Layout layout (Symbol atom _     ) = (Symbol atom layout)

type instance Get Format        (Symbol atom _     ) = Get Format atom

instance Phantom atom => Getter Atom     (Symbol atom layout) where get _ = phantom



--
-- ---------------------------------------------------------
-- -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! --
-- ---------------------------------------------------------
-- -- DEPRECIATED

type NameByDynamics dyn d = ByDynamics dyn Lit.String d
