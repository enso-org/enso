{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Term where

import qualified Luna.Prelude as P
import           Luna.Prelude hiding (String, Integer, Rational, Curry, Data)


import Data.Base                 (Base)
import Type.Applicative
import Data.Property
import Data.Phantom
import Data.Typeable             (TypeRep)
import OCI.IR.Layout.Format
import           OCI.IR.Layout.Class (LAYOUT, Merge, Bottom, type (<+>), Generalizable)
import Type.Bool
import Type.List (In)

import Data.Construction
import           Data.RTuple (List(Null, (:-:)))
import qualified Data.RTuple as List
import           Data.Reprx
import OCI.IR.Repr.Styles  (HeaderOnly)
import Data.TypeDesc


---------------------
-- === Terms === --
---------------------

-- === Definitions === --

newtype Term atom a = Term (TermDef atom a)
type family TermDef atom :: * -> *
makeWrapped ''Term


-- === Utils === --

type Terms atoms a = Term <$> atoms <*> '[a]


-- === Instances === --

deriving instance Show        (TermDef atom a) => Show        (Term atom a)
deriving instance Functor     (TermDef atom)   => Functor     (Term atom)
deriving instance Foldable    (TermDef atom)   => Foldable    (Term atom)
deriving instance Traversable (TermDef atom)   => Traversable (Term atom)




-- === Term isomorphisms === --

type AsTermDef s = TermDef (s # TermType) (s # LAYOUT)
type AsTerm    s = Term    (s # TermType) (s # LAYOUT)

type  IsTerm            s s' = (ToTerm s, FromTerm s')
type  IsTerm'           s    = IsTerm s s
class FromTerm          s where fromTerm          :: AsTerm s -> s
class UncheckedFromTerm s where uncheckedFromTerm :: AsTerm s -> s
class ToTerm            s where toTerm            :: s -> AsTerm s

instance FromTerm (Term atom layout) where fromTerm = id
instance ToTerm   (Term atom layout) where toTerm   = id

term  :: IsTerm  s s' => Iso  s s' (AsTerm s) (AsTerm s')
term' :: IsTerm' s    => Iso' s    (AsTerm s)
term  = iso toTerm fromTerm
term' = term

--
-- uncheckedFromTermDef :: UncheckedFromTerm s => AsTermDef s -> s
-- uncheckedFromTermDef = uncheckedFromTerm . Term

uncheckedFromTermDef :: (TermDef atom a ~ t, atom ~ (t # TermType), atom ~ (s # TermType), a ~ (s # LAYOUT), UncheckedFromTerm s) => t -> s
uncheckedFromTermDef = uncheckedFromTerm . mkTerm

mkTerm :: (TermDef atom a ~ t, atom ~ (t # TermType)) => t -> Term atom a
mkTerm = Term



-- === Selectors === --

data TERM = TERM deriving (Show)

type instance Access   TERM (Term atom layout) = Term atom layout
instance      Accessor TERM (Term atom layout) where
    access = id


-- === Instances === --

-- Properties

type instance TermTypeOf (Term atom _) = atom

type instance Access TermType        (Term atom _     ) = atom
type instance Update TermType atom   (Term _    layout) = (Term atom layout)

type instance Access LAYOUT        (Term _    layout) = layout
type instance Update LAYOUT layout (Term atom _     ) = (Term atom layout)

type instance Access Format        (Term atom _     ) = Access Format atom


-- Repr

instance KnownRepr a => Repr HeaderOnly (Term a l) where repr _ = fromString $ typeRepr @a




-- === Definition pragmas === --




data TermType

data TermicType a

-- FIXME[WD]: TermTypeOf should not be needed if we allow Data.Property.Access to result different kinds,
--            so we can replace TermTypesOf too.
type family TermTypeOf  a ::  *
type family TermTypesOf a :: [*]


-- === TermDesc === --

type TermDesc = TypeDescT TermType

getTermDesc :: forall a. KnownType (TermTypeOf a) => TermDesc
getTermDesc = getTypeDesc @(TermTypeOf a) ; {-# INLINE getTermDesc #-}

atomDescOf :: forall a. KnownType (TermTypeOf a) => a -> TermDesc
atomDescOf _ = getTermDesc @a ; {-# INLINE atomDescOf #-}


-- === Instances === --

type instance TermTypeOf      (TermicType a) =   TermicType a
type instance TermTypesOf     (TermicType a) = '[TermicType a]
type instance Access TermType (TermicType a) =   TermicType a
type instance TypeRepr        (TermicType a) =   TypeRepr   a




----


type instance Generalizable (TermicType a) (Form   b) = TermicType a `In` TermTypesOf (Form b)
type instance Generalizable (TermicType a) (TermicType b) = a == b


type instance Merge (Form   a) (TermicType b) = (Form a) <+> (TermicType b # Format)
type instance Merge (TermicType a) (Form   b) = (TermicType a # Format) <+> (Form b)

type instance Merge (TermicType a) (TermicType b) = If (a == b) (TermicType a) ((TermicType a # Format) <+> (TermicType b # Format))-- TODO: refactor
type instance Merge (TermicType a) ()         = TermicType a
type instance Merge ()         (TermicType a) = TermicType a

type instance Merge Bottom        (TermicType a) = Bottom
type instance Merge (TermicType a) Bottom        = Bottom
