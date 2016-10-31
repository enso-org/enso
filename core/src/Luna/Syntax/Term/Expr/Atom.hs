{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Term.Expr.Atom where

import Prelude.Luna hiding (String, Integer, Rational)


-------------------
-- === Types === --
-------------------


-- === Definitions === --

data Integer  = Integer  deriving (Show, Eq, Ord)
data Rational = Rational deriving (Show, Eq, Ord)
data String   = String   deriving (Show, Eq, Ord)

data Acc      = Acc      deriving (Show, Eq, Ord)
data App      = App      deriving (Show, Eq, Ord)
data Blank    = Blank    deriving (Show, Eq, Ord)
data Cons     = Cons     deriving (Show, Eq, Ord)
data Curry    = Curry    deriving (Show, Eq, Ord)
data Lam      = Lam      deriving (Show, Eq, Ord)
data Match    = Match    deriving (Show, Eq, Ord)
data Missing  = Missing  deriving (Show, Eq, Ord)
data Native   = Native   deriving (Show, Eq, Ord)
data Star     = Star     deriving (Show, Eq, Ord)
data Unify    = Unify    deriving (Show, Eq, Ord)
data Var      = Var      deriving (Show, Eq, Ord)


-- === Selectors === --

type family   Atoms a       :: [*]
type instance Atoms Acc     = '[Acc    ]
type instance Atoms App     = '[App    ]
type instance Atoms Blank   = '[Blank  ]
type instance Atoms Cons    = '[Cons   ]
type instance Atoms Curry   = '[Curry  ]
type instance Atoms Lam     = '[Lam    ]
type instance Atoms Match   = '[Match  ]
type instance Atoms Missing = '[Missing]
type instance Atoms Native  = '[Native ]
type instance Atoms Star    = '[Star   ]
type instance Atoms Unify   = '[Unify  ]
type instance Atoms Var     = '[Var    ]


-- === Instances === --

-- Default

instance Default Integer  where def = Integer  ; {-# INLINE def #-}
instance Default String   where def = String   ; {-# INLINE def #-}
instance Default Rational where def = Rational ; {-# INLINE def #-}

instance Default Acc      where def = Acc      ; {-# INLINE def #-}
instance Default App      where def = App      ; {-# INLINE def #-}
instance Default Blank    where def = Blank    ; {-# INLINE def #-}
instance Default Cons     where def = Cons     ; {-# INLINE def #-}
instance Default Curry    where def = Curry    ; {-# INLINE def #-}
instance Default Lam      where def = Lam      ; {-# INLINE def #-}
instance Default Match    where def = Match    ; {-# INLINE def #-}
instance Default Missing  where def = Missing  ; {-# INLINE def #-}
instance Default Native   where def = Native   ; {-# INLINE def #-}
instance Default Star     where def = Star     ; {-# INLINE def #-}
instance Default Unify    where def = Unify    ; {-# INLINE def #-}
instance Default Var      where def = Var      ; {-# INLINE def #-}

-- Repr

instance {-# OVERLAPPABLE #-} Repr s Integer  where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s Rational where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s String   where repr = fromString ∘ show ; {-# INLINE repr #-}

instance {-# OVERLAPPABLE #-} Repr s Acc      where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s App      where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s Blank    where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s Cons     where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s Curry    where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s Lam      where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s Match    where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s Missing  where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s Native   where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s Star     where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s Unify    where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s Var      where repr = fromString ∘ show ; {-# INLINE repr #-}
