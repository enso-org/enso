{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP                  #-}

module Luna.Syntax.Term.Expr.Atom where

import Prelude.Luna hiding (String, Integer, Rational, Curry)
import Data.Base

-------------------
-- === Atoms === --
-------------------

-- === Definition pragmas === --

#define DEFINE_ATOM(name)                                      \
data name = name deriving (Show, Eq, Ord)                     ;\
type instance Atoms name = '[name]                            ;\
instance Default name where {def = name ; {-# INLINE def #-}} ;\
instance {-# OVERLAPPABLE #-} Repr s name where {repr = fromString ∘ show ; {-# INLINE repr #-}}


-- === Queries === --

type family Atoms a :: [*]


-- === Definitions === --

DEFINE_ATOM(Integer )
DEFINE_ATOM(Rational)
DEFINE_ATOM(String  )

DEFINE_ATOM(Acc    )
DEFINE_ATOM(App    )
DEFINE_ATOM(Blank  )
DEFINE_ATOM(Cons   )
DEFINE_ATOM(Curry  )
DEFINE_ATOM(Lam    )
DEFINE_ATOM(Match  )
DEFINE_ATOM(Missing)
DEFINE_ATOM(Native )
DEFINE_ATOM(Star   )
DEFINE_ATOM(Unify  )
DEFINE_ATOM(Var    )
