{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Expr.Term.Class (module Luna.IR.Expr.Term.Class) where

import qualified Luna.Prelude as P
import           Luna.Prelude hiding (String, Integer, Rational, Curry, Data)

import Luna.IR.Expr.Atom (Atom, AtomOf)

import Data.Base                 (Base)
import Type.Applicative
import Data.Property
import Data.Phantom
import Data.Typeable             (TypeRep)
import Luna.IR.Expr.Format
import qualified Luna.IR.Expr.Layout as Layout
import           Luna.IR.Expr.Layout (LAYOUT)

import qualified Old.Luna.Syntax.Term.Expr.Lit  as Lit

import Data.Construction
import           Data.RTuple (List(Null, (:-:)))
import qualified Data.RTuple as List
import           Data.Reprx
import Luna.IR.Repr.Styles  (HeaderOnly)


---------------------
-- === Terms === --
---------------------

-- === Definitions === --

type family Term2 atom layout = k | k -> atom layout
data family Term  atom  layout
type        Terms atoms layout = Term <$> atoms <*> '[layout]

data family UniTerm a
class     IsUniTerm t l where
    uniTerm :: Term t l -> UniTerm l

type family InputsType a
class HasInputs a where
    inputList :: a -> [InputsType a]

class ModifiesFields a where
    modifyFields :: (FieldsType a -> FieldsType a) -> a -> a


-- === Term isomorphisms === --

type AsTerm s = Term (Access Atom s) (Access LAYOUT s)

type  IsTerm            s s' = (ToTerm s, FromTerm s')
type  IsTerm'           s    = IsTerm s s
class FromTerm          s where fromTerm          :: AsTerm s -> s
class UncheckedFromTerm s where uncheckedFromTerm :: AsTerm s -> s
class ToTerm            s where toTerm            :: s -> AsTerm s

instance FromTerm (Term atom layout) where fromTerm = id ; {-# INLINE fromTerm #-}
instance ToTerm   (Term atom layout) where toTerm   = id ; {-# INLINE toTerm   #-}

term :: IsTerm s s' => Iso s s' (AsTerm s) (AsTerm s')
term = iso toTerm fromTerm ; {-# INLINE term #-}

term' :: IsTerm' s => Iso' s (AsTerm s)
term' = term ; {-# INLINE term' #-}



-- === Selectors === --

data TERM = TERM deriving (Show)

type instance Access   TERM (Term atom layout) = Term atom layout
instance      Accessor TERM (Term atom layout) where
    access = id ; {-# INLINE access #-}


-- === Instances === --

-- Properties

type instance AtomOf (Term atom _) = atom

type instance Access Atom          (Term atom _     ) = atom
type instance Update Atom   atom   (Term _    layout) = (Term atom layout)

type instance Access LAYOUT        (Term _    layout) = layout
type instance Update LAYOUT layout (Term atom _     ) = (Term atom layout)

type instance Access Format        (Term atom _     ) = Access Format atom


-- Repr

instance KnownRepr a => Repr HeaderOnly (Term a l) where repr _ = fromString $ typeRepr @a ; {-# INLINE repr #-}
