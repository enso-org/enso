{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE RecursiveDo            #-}

module Luna.Syntax.Model.Network.Term where

import Prologue hiding (Getter, Setter, Cons, Num, cons)

import           Luna.Syntax.Model.Network.Class
import           Control.Monad.Event
import           Data.Prop
import           Data.Layer.Cover
import           Data.Record                    (HasRecord, RecordOf, IsRecord, asRecord, SmartCons, Variant, MapTryingElemList_, withElement_, Props)
import qualified Data.Record                    as Record
import           Data.Reprx                     (Repr, repr)
import           Luna.Syntax.AST.Term           hiding (Val, Lit, Thunk, Expr, Draft)
import           Luna.Syntax.Model.Layer
import           Luna.Evaluation.Runtime        as Runtime
import           Luna.Syntax.AST.Arg
import qualified Luna.Evaluation.Model          as Model
import Data.Graph.Model.Edge
import Data.Graph
import Data.Prop

-- TODO[WD]: refactor the code to some kind of Luna/Evaluation/Model

---------------------------------------
-- === Network layout definition === --
---------------------------------------

type instance Layout (Network ls) term rt = Ref Edge $ Link (ls :<: TermWrapper term rt)

type family TermWrapper (a :: *) :: * -> [*] -> *

---------------------------
-- === Network Terms === --
---------------------------

-- === Definitions === --

data    Raw      (ls :: [*]) = Raw Data                                 deriving (Show, Eq)
newtype Lit   rt (ls :: [*]) = Lit   (Term (Network ls) Model.Lit   rt) deriving (Show, Eq)
newtype Val   rt (ls :: [*]) = Val   (Term (Network ls) Model.Val   rt) deriving (Show, Eq)
newtype Thunk rt (ls :: [*]) = Thunk (Term (Network ls) Model.Thunk rt) deriving (Show, Eq)
newtype Expr  rt (ls :: [*]) = Expr  (Term (Network ls) Model.Expr  rt) deriving (Show, Eq)
newtype Draft rt (ls :: [*]) = Draft (Term (Network ls) Model.Draft rt) deriving (Show, Eq)


-- === Instances === --

-- Wrappers

makeWrapped ''Raw
makeWrapped ''Lit
makeWrapped ''Val
makeWrapped ''Thunk
makeWrapped ''Expr
makeWrapped ''Draft

type instance Unlayered (Raw      ls) = Unwrapped (Raw      ls)
type instance Unlayered (Lit   rt ls) = Unwrapped (Lit   rt ls)
type instance Unlayered (Val   rt ls) = Unwrapped (Val   rt ls)
type instance Unlayered (Thunk rt ls) = Unwrapped (Thunk rt ls)
type instance Unlayered (Expr  rt ls) = Unwrapped (Expr  rt ls)
type instance Unlayered (Draft rt ls) = Unwrapped (Draft rt ls)

instance Layered (Raw      ls)
instance Layered (Lit   rt ls)
instance Layered (Val   rt ls)
instance Layered (Thunk rt ls)
instance Layered (Expr  rt ls)
instance Layered (Draft rt ls)

-- Term bindings

type instance TermWrapper Model.Lit   = Lit
type instance TermWrapper Model.Val   = Val
type instance TermWrapper Model.Thunk = Thunk
type instance TermWrapper Model.Expr  = Expr
type instance TermWrapper Model.Draft = Draft

-- Term origins

type instance TermOf (Lit   rt ls) = TermOf (Unwrapped (Lit   rt ls))
type instance TermOf (Val   rt ls) = TermOf (Unwrapped (Val   rt ls))
type instance TermOf (Thunk rt ls) = TermOf (Unwrapped (Thunk rt ls))
type instance TermOf (Expr  rt ls) = TermOf (Unwrapped (Expr  rt ls))
type instance TermOf (Draft rt ls) = TermOf (Unwrapped (Draft rt ls))

-- Records

type instance RecordOf (Lit   rt ls) = RecordOf (Unwrapped (Lit   rt ls))
type instance RecordOf (Val   rt ls) = RecordOf (Unwrapped (Val   rt ls))
type instance RecordOf (Thunk rt ls) = RecordOf (Unwrapped (Thunk rt ls))
type instance RecordOf (Expr  rt ls) = RecordOf (Unwrapped (Expr  rt ls))
type instance RecordOf (Draft rt ls) = RecordOf (Unwrapped (Draft rt ls))

instance IsRecord (Lit   rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Val   rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Thunk rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Expr  rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Draft rt ls) where asRecord = wrapped' ∘ asRecord

instance HasRecord (Lit   rt ls)
instance HasRecord (Val   rt ls)
instance HasRecord (Thunk rt ls)
instance HasRecord (Expr  rt ls)
instance HasRecord (Draft rt ls)

-- Runtime models

type instance Runtime.Model (Lit   rt ls) = Runtime.Model (Unwrapped (Lit   rt ls))
type instance Runtime.Model (Val   rt ls) = Runtime.Model (Unwrapped (Val   rt ls))
type instance Runtime.Model (Thunk rt ls) = Runtime.Model (Unwrapped (Thunk rt ls))
type instance Runtime.Model (Expr  rt ls) = Runtime.Model (Unwrapped (Expr  rt ls))
type instance Runtime.Model (Draft rt ls) = Runtime.Model (Unwrapped (Draft rt ls))

-- Layouts

type instance LayoutType (Raw      ls) = Network ls
type instance LayoutType (Lit   rt ls) = Network ls
type instance LayoutType (Val   rt ls) = Network ls
type instance LayoutType (Thunk rt ls) = Network ls
type instance LayoutType (Expr  rt ls) = Network ls
type instance LayoutType (Draft rt ls) = Network ls

-- Conversions

instance Castable (Raw      ls) (Raw      ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Lit   rt ls) (Lit   rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Val   rt ls) (Val   rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Thunk rt ls) (Thunk rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Expr  rt ls) (Expr  rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Draft rt ls) (Draft rt ls) where cast = id ; {-# INLINE cast #-}

instance Castable (Lit   rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Val   rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Thunk rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Expr  rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Draft rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}

instance Castable (Raw ls) (Lit   rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Val   rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Thunk rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Expr  rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Draft rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}

-- Representations

instance {-# OVERLAPPABLE #-}                                     Repr s (Raw      ls) where repr = const "Raw"
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Lit   rt ls)) => Repr s (Lit   rt ls) where repr = repr ∘ unwrap'
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Val   rt ls)) => Repr s (Val   rt ls) where repr = repr ∘ unwrap'
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Thunk rt ls)) => Repr s (Thunk rt ls) where repr = repr ∘ unwrap'
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Expr  rt ls)) => Repr s (Expr  rt ls) where repr = repr ∘ unwrap'
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Draft rt ls)) => Repr s (Draft rt ls) where repr = repr ∘ unwrap'

-- Properties

type instance Prop a (Lit   rt ls) = Prop a (Unwrapped (Lit   rt ls))
type instance Prop a (Val   rt ls) = Prop a (Unwrapped (Val   rt ls))
type instance Prop a (Thunk rt ls) = Prop a (Unwrapped (Thunk rt ls))
type instance Prop a (Expr  rt ls) = Prop a (Unwrapped (Expr  rt ls))
type instance Prop a (Draft rt ls) = Prop a (Unwrapped (Draft rt ls))

instance SubGetter a (Lit   rt ls) => Getter a (Lit   rt ls)
instance SubGetter a (Val   rt ls) => Getter a (Val   rt ls)
instance SubGetter a (Thunk rt ls) => Getter a (Thunk rt ls)
instance SubGetter a (Expr  rt ls) => Getter a (Expr  rt ls)
instance SubGetter a (Draft rt ls) => Getter a (Draft rt ls)

instance SubSetter a (Lit   rt ls) => Setter a (Lit   rt ls)
instance SubSetter a (Val   rt ls) => Setter a (Val   rt ls)
instance SubSetter a (Thunk rt ls) => Setter a (Thunk rt ls)
instance SubSetter a (Expr  rt ls) => Setter a (Expr  rt ls)
instance SubSetter a (Draft rt ls) => Setter a (Draft rt ls)

