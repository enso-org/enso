{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE RecursiveDo            #-}

module Old.Luna.Syntax.Model.Network.Term where

import Prologue hiding (Getter, Setter, Cons, Num, cons)

import           Old.Luna.Syntax.Model.Network.Class
import           Old.Control.Monad.Event
import           Old.Data.Prop
import           Data.Layer_OLD.Cover_OLD
import           Data.Record                    (HasRecord, RecordOf, IsRecord, asRecord, SmartCons, Variant, MapTryingElemList_, withElement_, Props)
import qualified Data.Record                    as Record
import           Data.Reprx                     (Repr, repr)
import           Old.Luna.Syntax.Term.Class
import           Old.Luna.Syntax.Model.Layer
import           Old.Luna.Runtime.Dynamics          (Dynamics_OLD)
import qualified Luna.IR.Term.Format          as Format
import           Old.Luna.Syntax.Term               hiding (Layout, Term, Data, Link, Ref)
import Old.Data.Graph.Model.Edge
import Old.Data.Graph
import Old.Data.Prop

-- TODO[WD]: refactor the code to some kind of Luna/Evaluation/Model

---------------------------------------
-- === Network layout definition === --
---------------------------------------

type instance Layout (Network ls) term rt = Ref Edge (Link (ls :<: TermWrapper term rt))

type family TermWrapper (a :: *) :: * -> [*] -> *

---------------------------
-- === Network Terms === --
---------------------------

-- === Definitions === --

data    Raw       (ls :: [*]) = Raw Data                                     deriving (Show)
newtype Lit    rt (ls :: [*]) = Lit    (Term (Network ls) Format.Literal rt) deriving (Show, Eq, Generic, NFData)
newtype Val    rt (ls :: [*]) = Val    (Term (Network ls) Format.Value   rt) deriving (Show, Eq, Generic, NFData)
newtype Thunk  rt (ls :: [*]) = Thunk  (Term (Network ls) Format.Thunk   rt) deriving (Show, Eq, Generic, NFData)
newtype Phrase rt (ls :: [*]) = Phrase (Term (Network ls) Format.Phrase  rt) deriving (Show, Eq, Generic, NFData)
newtype Draft  rt (ls :: [*]) = Draft  (Term (Network ls) Format.Draft   rt) deriving (Show, Eq, Generic, NFData)


-- === Instances === --

-- Wrappers

makeWrapped ''Raw
makeWrapped ''Lit
makeWrapped ''Val
makeWrapped ''Thunk
makeWrapped ''Phrase
makeWrapped ''Draft

type instance Unlayered (Raw       ls) = Unwrapped (Raw       ls)
type instance Unlayered (Lit    rt ls) = Unwrapped (Lit    rt ls)
type instance Unlayered (Val    rt ls) = Unwrapped (Val    rt ls)
type instance Unlayered (Thunk  rt ls) = Unwrapped (Thunk  rt ls)
type instance Unlayered (Phrase rt ls) = Unwrapped (Phrase rt ls)
type instance Unlayered (Draft  rt ls) = Unwrapped (Draft  rt ls)

instance Layered (Raw       ls)
instance Layered (Lit    rt ls)
instance Layered (Val    rt ls)
instance Layered (Thunk  rt ls)
instance Layered (Phrase rt ls)
instance Layered (Draft  rt ls)

-- Term bindings

type instance TermWrapper Format.Literal = Lit
type instance TermWrapper Format.Value   = Val
type instance TermWrapper Format.Thunk   = Thunk
type instance TermWrapper Format.Phrase  = Phrase
type instance TermWrapper Format.Draft   = Draft

-- Term origins

--type instance TermOf (Lit   rt ls) = TermOf (Unwrapped (Lit   rt ls))
--type instance TermOf (Val   rt ls) = TermOf (Unwrapped (Val   rt ls))
--type instance TermOf (Thunk rt ls) = TermOf (Unwrapped (Thunk rt ls))
--type instance TermOf (Phrase  rt ls) = TermOf (Unwrapped (Phrase  rt ls))
--type instance TermOf (Draft rt ls) = TermOf (Unwrapped (Draft rt ls))

type instance TermOf (Lit    rt ls) = Lit    rt ls
type instance TermOf (Val    rt ls) = Val    rt ls
type instance TermOf (Thunk  rt ls) = Thunk  rt ls
type instance TermOf (Phrase rt ls) = Phrase rt ls
type instance TermOf (Draft  rt ls) = Draft  rt ls

-- Records

type instance RecordOf (Lit    rt ls) = RecordOf (Unwrapped (Lit    rt ls))
type instance RecordOf (Val    rt ls) = RecordOf (Unwrapped (Val    rt ls))
type instance RecordOf (Thunk  rt ls) = RecordOf (Unwrapped (Thunk  rt ls))
type instance RecordOf (Phrase rt ls) = RecordOf (Unwrapped (Phrase rt ls))
type instance RecordOf (Draft  rt ls) = RecordOf (Unwrapped (Draft  rt ls))

instance IsRecord (Lit    rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Val    rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Thunk  rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Phrase rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Draft  rt ls) where asRecord = wrapped' ∘ asRecord

instance HasRecord (Lit    rt ls)
instance HasRecord (Val    rt ls)
instance HasRecord (Thunk  rt ls)
instance HasRecord (Phrase rt ls)
instance HasRecord (Draft  rt ls)

-- Runtime models

type instance Dynamics_OLD (Lit    rt ls) = Dynamics_OLD (Unwrapped (Lit    rt ls))
type instance Dynamics_OLD (Val    rt ls) = Dynamics_OLD (Unwrapped (Val    rt ls))
type instance Dynamics_OLD (Thunk  rt ls) = Dynamics_OLD (Unwrapped (Thunk  rt ls))
type instance Dynamics_OLD (Phrase rt ls) = Dynamics_OLD (Unwrapped (Phrase rt ls))
type instance Dynamics_OLD (Draft  rt ls) = Dynamics_OLD (Unwrapped (Draft  rt ls))

-- Layouts

type instance LayoutType (Raw       ls) = Network ls
type instance LayoutType (Lit    rt ls) = Network ls
type instance LayoutType (Val    rt ls) = Network ls
type instance LayoutType (Thunk  rt ls) = Network ls
type instance LayoutType (Phrase rt ls) = Network ls
type instance LayoutType (Draft  rt ls) = Network ls

-- Conversions

instance Castable (Raw       ls) (Raw       ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Lit    rt ls) (Lit    rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Val    rt ls) (Val    rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Thunk  rt ls) (Thunk  rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Phrase rt ls) (Phrase rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Draft  rt ls) (Draft  rt ls) where cast = id ; {-# INLINE cast #-}

instance Castable (Lit    rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Val    rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Thunk  rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Phrase rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Draft  rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}

instance Castable (Raw ls) (Lit    rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Val    rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Thunk  rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Phrase rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Draft  rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}

-- Representations

instance {-# OVERLAPPABLE #-}                                      Repr s (Raw       ls) where repr = const "Raw"
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Lit    rt ls)) => Repr s (Lit    rt ls) where repr = repr ∘ unwrap'
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Val    rt ls)) => Repr s (Val    rt ls) where repr = repr ∘ unwrap'
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Thunk  rt ls)) => Repr s (Thunk  rt ls) where repr = repr ∘ unwrap'
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Phrase rt ls)) => Repr s (Phrase rt ls) where repr = repr ∘ unwrap'
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Draft  rt ls)) => Repr s (Draft  rt ls) where repr = repr ∘ unwrap'

-- Properties

type instance Prop a (Lit    rt ls) = Prop a (Unwrapped (Lit    rt ls))
type instance Prop a (Val    rt ls) = Prop a (Unwrapped (Val    rt ls))
type instance Prop a (Thunk  rt ls) = Prop a (Unwrapped (Thunk  rt ls))
type instance Prop a (Phrase rt ls) = Prop a (Unwrapped (Phrase rt ls))
type instance Prop a (Draft  rt ls) = Prop a (Unwrapped (Draft  rt ls))

instance SubGetter a (Lit    rt ls) => Getter a (Lit    rt ls)
instance SubGetter a (Val    rt ls) => Getter a (Val    rt ls)
instance SubGetter a (Thunk  rt ls) => Getter a (Thunk  rt ls)
instance SubGetter a (Phrase rt ls) => Getter a (Phrase rt ls)
instance SubGetter a (Draft  rt ls) => Getter a (Draft  rt ls)

instance SubSetter a (Lit    rt ls) => Setter a (Lit    rt ls)
instance SubSetter a (Val    rt ls) => Setter a (Val    rt ls)
instance SubSetter a (Thunk  rt ls) => Setter a (Thunk  rt ls)
instance SubSetter a (Phrase rt ls) => Setter a (Phrase rt ls)
instance SubSetter a (Draft  rt ls) => Setter a (Draft  rt ls)
