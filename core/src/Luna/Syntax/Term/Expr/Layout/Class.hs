{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds            #-}

module Luna.Syntax.Term.Expr.Layout.Class where


import Prelude.Luna
import Data.Property
import Luna.Syntax.Term.Expr.Format
import Luna.Syntax.Term.Expr.Atom

import Unsafe.Coerce (unsafeCoerce)
import GHC.TypeLits (ErrorMessage(Text, ShowType, (:<>:)), TypeError)
import Type.Error
import Type.List (In)
import Data.Reprx
import Type.Bool


-- FIXME: remove or refactor
data Named n a
data Name = Name deriving (Show)
data Type




---------------------
-- === Layouts === --
---------------------

data Layout = Layout deriving (Show)
type family LayoutOf a

type family DefaultLayout (p :: k)


-- === Scoping === --

type family Merge     a b
type family Simplify  l
type family Universal a

class                         Generalize a b
instance {-# OVERLAPPABLE #-} Generalize a a


-- === Utils === --

type GeneralizableError a b = Sentence
                            ( 'Text  "Cannot generalize"
                        :</>: Ticked ('Text (TypeRepr a))
                        :</>: 'Text  "to"
                        :</>: Ticked ('Text (TypeRepr b)))

generalize :: Generalize a b => a -> b
generalize = unsafeCoerce ; {-# INLINE generalize #-}

universal :: a -> Universal a
universal = unsafeCoerce ; {-# INLINE universal #-}


-- === Instances === --

-- Universal formats
type instance Universal (Form a) = Draft

-- Generalize

instance {-# OVERLAPPABLE #-} Assert ((Atomic a) `In` (Atoms (Form f)))
                                     (GeneralizableError (Atomic a) (Form f))
                           => Generalize (Atomic a) (Form f)

instance {-# OVERLAPPABLE #-} (TypeError (GeneralizableError (Atomic a) (Atomic b)))
                           => Generalize (Atomic a) (Atomic b)
instance {-# OVERLAPPABLE #-} Generalize (Atomic a) (Atomic a)

-- Merge

type instance Merge (Form   a) ()         = Form a
type instance Merge ()         (Form   b) = Form b
type instance Merge (Form   a) (Form   b) = If (Form a > Form b) (Form a) (Form b)
type instance Merge (Form   a) (Atomic b) = Merge (Form a) (Atomic b # Format)
type instance Merge (Atomic a) (Form   b) = Merge (Atomic a # Format) (Form b)

type instance Merge (Atomic a) (Atomic b) = If (a == b) (Atomic a) (Merge (Atomic a # Format) (Atomic b # Format))-- TODO: refactor
type instance Merge (Atomic a) ()         = Atomic a
type instance Merge ()         (Atomic a) = Atomic a
