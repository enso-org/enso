{-# LANGUAGE UndecidableInstances #-}
{-# BOOSTER  Templates            #-}

module Luna.Syntax.Term.Expr.Atom where

import Prelude.Luna hiding (String, Integer, Rational, Curry)
import Data.Base
import Data.Phantom

-------------------
-- === Atoms === --
-------------------

-- === Definition pragmas === --

define name = {{

data {name} = {name} deriving (Show, Eq, Ord)

type instance Atoms {name} = '[{name}]

instance Default {name} where
    def = {name} ; {-# INLINE def #-}

instance Phantom {name} where
    phantom = {name} ; {-# INLINE phantom #-}

instance {-# OVERLAPPABLE #-} Repr s {name} where
    repr = fromString ∘ show ; {-# INLINE repr #-}

}}


-- === Queries === --

data Atom = Atom deriving (Show)

type family Atoms a :: [*]


-- === Definitions === --

define Integer
define Rational
define String

define Acc
define App
define Blank
define Cons
define Curry
define Lam
define Match
define Missing
define Native
define Star
define Unify
define Var
