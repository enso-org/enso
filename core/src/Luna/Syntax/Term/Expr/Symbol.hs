{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Term.Expr.Symbol where

import Prelude.Luna hiding (Curry)

import Data.Base
import Luna.Syntax.Term.Function (Arg)
import Luna.Pretty.Styles  (HeaderOnly, StaticNameOnly(StaticNameOnly))

import qualified Luna.Syntax.Term.Expr.Lit as Lit
import Type.Applicative


-------------------
-- === Types === --
-------------------

-- === Definitions === --

data Var    = Var    deriving (Show, Eq, Ord)
data Cons   = Cons   deriving (Show, Eq, Ord)
data Acc    = Acc    deriving (Show, Eq, Ord)
data App    = App    deriving (Show, Eq, Ord)
data Curry  = Curry  deriving (Show, Eq, Ord)
data Unify  = Unify  deriving (Show, Eq, Ord)
data Match  = Match  deriving (Show, Eq, Ord)
data Lam    = Lam    deriving (Show, Eq, Ord)
data Native = Native deriving (Show, Eq, Ord)
data Blank  = Blank  deriving (Show, Eq, Ord)

-- === Instances === --

instance Default Var    where def = Var    ; {-# INLINE def #-}
instance Default Cons   where def = Cons   ; {-# INLINE def #-}
instance Default Acc    where def = Acc    ; {-# INLINE def #-}
instance Default App    where def = App    ; {-# INLINE def #-}
instance Default Curry  where def = Curry  ; {-# INLINE def #-}
instance Default Unify  where def = Unify  ; {-# INLINE def #-}
instance Default Match  where def = Match  ; {-# INLINE def #-}
instance Default Lam    where def = Lam    ; {-# INLINE def #-}
instance Default Native where def = Native ; {-# INLINE def #-}
instance Default Blank  where def = Blank  ; {-# INLINE def #-}

instance {-# OVERLAPPABLE #-} Repr s Var    where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s Cons   where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s Acc    where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s App    where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s Curry  where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s Unify  where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s Match  where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s Lam    where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s Native where repr = fromString ∘ show ; {-# INLINE repr #-}
instance {-# OVERLAPPABLE #-} Repr s Blank  where repr = fromString ∘ show ; {-# INLINE repr #-}
