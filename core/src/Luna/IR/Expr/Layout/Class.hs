{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Expr.Layout.Class where


import Luna.Prelude as Prelude
import Data.Property
import Luna.IR.Expr.Format
import Luna.IR.Expr.Atom

import Unsafe.Coerce (unsafeCoerce)
import GHC.TypeLits (ErrorMessage(Text, ShowType, (:<>:)), TypeError)
import Type.Error
import Type.List (In)
import Data.Reprx
import Type.Bool
import Type.Inference (KnownTypeT, runInferenceT2)


-- FIXME: remove or refactor
data Named n a
data Name = Name deriving (Show)




---------------------
-- === Layouts === --
---------------------

data Layout = Layout deriving (Show)
type family LayoutOf a

type family DefaultLayout p


-- === Scoping === --

type family Current a

type family Merge       a b
type family Simplify    l
type family Universal   a
type family Abstract    a
-- type family Specialized t spec layout

type family AsSubLayout t l
type family LiteralLayout t layout
type family AtomLayout    t layout

type l <+> r = Merge              l r
type l %>  r = LiteralLayout      l r


class                         Generalize a b
instance {-# OVERLAPPABLE #-} Generalize a a

type family Merges


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

abstract :: a -> Abstract a
abstract = unsafeCoerce ; {-# INLINE abstract #-}


baseLayout :: forall t m a. KnownTypeT Layout t m a -> m a
baseLayout = runInferenceT2 @Layout


type Layouted l = KnownTypeT Layout (DefaultLayout l)
layouted :: forall l m a. Layouted l m a -> m a
layouted = baseLayout @(DefaultLayout l)


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
instance {-# OVERLAPPABLE #-} (Assert' (Form b > Form a)) => Generalize (Form a) (Form b)
instance                                                     Generalize (Form a) (Form a)



-- Merge

type instance Merge (Form   a) ()         = Form a
type instance Merge ()         (Form   b) = Form b
type instance Merge (Form   a) (Form   b) = If (Form a > Form b) (Form a) (Form b)
type instance Merge (Form   a) (Atomic b) = Merge (Form a) (Atomic b # Format)
type instance Merge (Atomic a) (Form   b) = Merge (Atomic a # Format) (Form b)

type instance Merge (Atomic a) (Atomic b) = If (a == b) (Atomic a) (Merge (Atomic a # Format) (Atomic b # Format))-- TODO: refactor
type instance Merge (Atomic a) ()         = Atomic a
type instance Merge ()         (Atomic a) = Atomic a



-- === Sub === --

type family   Sub t a
-- type instance Sub t (Form   f) = Form   f
-- type instance Sub t (Atomic a) = Atomic a





-----------------
-- === Any === --
-----------------

data Any
type instance Sub t Any = Any
