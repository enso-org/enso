{-# LANGUAGE CPP                    #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}




module Luna.Syntax.Term.Expr.Class where


import           Prelude.Luna                 hiding (Field2, Enum, Num, Swapped, Curry, String, Integer, Rational, Symbol, Index, Data, Field)
import qualified Prelude.Luna                 as P

import           Data.Abstract
import           Data.Base
import           Data.Record                  hiding (Layout, Variants, Match, Cons, Value)
import qualified Data.Record                  as Record
import           Type.Cache.TH                (assertTypesEq, cacheHelper, cacheType)
import           Type.Container               hiding (Empty, FromJust)
import           Type.Map

import           Data.Typeable                (splitTyConApp, tyConName, typeRepTyCon)
import           Luna.Runtime.Dynamics      (Dynamics, Dynamic, Static, SubDynamics, SubSemiDynamics, ByDynamics)
import qualified Luna.Runtime.Dynamics      as Dynamics
import           Luna.Pretty.Styles
import           Luna.Syntax.Term.Function.Argument
import qualified Data.Reprx                   as Repr
import           Type.Bool
import           Luna.Syntax.Term.Expr.Format
import Luna.Syntax.Term.Expr.Symbol (Sym, Symbol, IsSymbol, symbol, FromSymbol, fromSymbol, ToSymbol, toSymbol)
import qualified Luna.Syntax.Term.Expr.Symbol2 as S2
import qualified Luna.Syntax.Term.Expr.Symbol.Named as N
import Luna.Syntax.Term.Expr.Atom

-- import Data.Shell               as Shell hiding (Access)
import Data.Record.Model.Masked as X (TermRecord, VGRecord2, Store2(Store2), Slot(Slot), Enum(Enum))
import Type.Monoid
import Type.Applicative

import Prologue.Unsafe (error)
-- import Luna.Syntax.Term.Expr (NameByDynamics)
import qualified Luna.Syntax.Term.Expr.Symbol as Symbol
import qualified Data.RTuple as List
import Type.Promotion    (KnownNats, natVals)
import Data.Bits         (setBit, zeroBits)

import Data.Container.Hetero (Elems)
import Data.RTuple (List, Empty, empty)
import Data.Record.Model.Masked (encode2, EncodeStore, encodeStore, Mask, encodeNat, encodeData2, checkData2, decodeData2, Raw(Raw), unsafeRestore, decodeNat)
import           Data.RTuple (TMap(..), empty, Assoc(..), Assocs, (:=:)) -- refactor empty to another library

import GHC.TypeLits (ErrorMessage(Text, ShowType, (:<>:)))
import Type.Error
import Control.Monad.State
import Control.Lens.Property hiding (get)
import qualified Control.Lens.Property as Prop
import GHC.Stack (HasCallStack, callStack, prettyCallStack, withFrozenCallStack)
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as V
import Control.Monad.ST (ST, runST)
import Type.List (Index, Size)
import Type.Maybe (FromJust)
import Data.Phantom
import Unsafe.Coerce     (unsafeCoerce)
import Type.Relation (SemiSuper)
import Luna.Syntax.Term.Expr.Symbol.Hidden
import qualified Luna.Syntax.Term.Expr.Layout as Layout
import Luna.Syntax.Term.Expr.Layout (Layout, Name, Prim, Uniform)
-- import Data.Graph.Model.Edge (Edge) -- Should be removed as too deep dependency?
-- data {-kind-} Layout dyn form = Layout dyn form deriving (Show)
--
-- type instance Get Dynamics (Layout dyn form) = dyn
-- type instance Get Format   (Layout dyn form) = form




data a := b

type instance Get t (l  := v ': ls) = If (t == l) v (Get t ls)
type instance Get t (l ':= v ': ls) = If (t == l) v (Get t ls)



type PossibleVariants = [Acc, App, Blank, Cons, Curry, Lam, Match, Missing, Native, Star, Unify, Var]
type PossibleFormats  = [Literal, Value, Thunk, Phrase, Draft]





-- === Properties === --

-- TODO: refactor
data Data     = Data     deriving (Show)
data System   = System   deriving (Show)
data TermType = TermType deriving (Show)




----------------------
-- === TermData === --
----------------------

-- === Definition === --

type TermStoreSlots = '[ Atom ':= Enum, Format ':= Mask, Sym ':= Raw ]
type TermStore = Store2 TermStoreSlots

newtype TermData sys model = TermData TermStore deriving (Show)
makeWrapped ''TermData



-------------------
-- === Stack === --
-------------------

-- === Definition === --

data Stack layers (t :: ★ -> ★) where
    SLayer :: t l -> Stack ls t -> Stack (l ': ls) t
    SNull  :: Stack '[] t


-- === Instances === --

instance Show (Stack '[] t) where
    show _ = ")"

instance (Show (t l), Show (Stack ls t)) => Show (Stack (l ': ls) t) where
    show (SLayer l ls) = show l <> ", " <> show ls



------------------
-- === Expr === --
------------------

-- === Definitions === --

data Expr      t layers layout    = Expr (TermStack t layers layout) TermStore
type AnyExpr   t layers           = Expr t layers (Uniform Draft)
type PrimExpr  t layers name atom = Expr t layers (Prim name atom)
type PrimExpr' t layers      atom = PrimExpr t layers () atom

type TermStack t layers layout = Stack layers (Layer (Expr t layers layout))


-- === Properties === --

type instance Get Layout        (Expr _ _ layout) = layout
type instance Set Layout layout (Expr t layers _) = Expr t layers layout
type instance Get Data          (Expr _ _ _)      = TermStore
type instance Get TermType      (Expr t _ _)      = t
-- type instance Get Layers      (Expr _ layers _) = Proxy layers -- FIXME: when using ghc >= 8.0.1-head


instance Getter Data (Expr t layers layout) where get (Expr _ s) = s ; {-# INLINE get #-}


-- === Instances === --

type instance Binder (Expr t _ _)               = Binder t
type instance Linker (Expr t _ _) (Expr t' _ _) = Linker t t'

type instance Sub s (Expr t layers layout) = Expr t layers (Sub s layout)

deriving instance Show (TermStack t layers layout) => Show (Expr t layers layout)


--------------------
-- === Layers === --
--------------------

-- === Definition === --

data family Layer  t l

class LayerCons m l where
    consLayer :: forall t. m (Layer t l)


-- === Parameters === --

data Layers

-- === Construction === --

class    Monad m                          => LayersCons ls        m where buildLayers :: forall t. m (Stack ls (Layer t))
instance Monad m                          => LayersCons '[]       m where buildLayers = return SNull
instance (LayersCons ls m, LayerCons m l) => LayersCons (l ': ls) m where buildLayers = SLayer <$> consLayer <*> buildLayers







-------------------------
-- === Connections === --
-------------------------

-- === Definitions === --

type family Binder      tgt :: * -> *
type family Linker  src tgt :: * -> * -> *
type        Binder'     tgt = Binder     tgt     tgt
type        Linker' src tgt = Linker src tgt src tgt

newtype Binding     tgt = Binding (Binder' tgt)
newtype Link    src tgt = Link    (Linker' src tgt)
makeWrapped ''Binding
makeWrapped ''Link


-- === Classes === --

class Monad m => Bindable     tgt m where bind ::        tgt -> m (Binding     tgt)
class Monad m => Linkable src tgt m where link :: src -> tgt -> m (Link    src tgt)


-- === Utils === --

type SubLink    c t = Link t  (Sub c t)
type SubBinding c t = Binding (Sub c t)


-- === Instances === --

deriving instance Show (Unwrapped (Binding     tgt)) => Show (Binding     tgt)
deriving instance Show (Unwrapped (Link    src tgt)) => Show (Link    src tgt)

type instance Deconstructed (Binding a) = a
type instance Get p   (Binding a) = Get p a
type instance Set p v (Binding a) = Binding (Set p v a)









newtype ExprSymbol atom t = ExprSymbol (N.NamedSymbol atom (SubLink Name t) (SubLink Atom t))
makeWrapped ''ExprSymbol

type instance Get p (ExprSymbol atom t) = Get p (Unwrapped (ExprSymbol atom t))


instance ValidateLayout (Get Layout t) Atom atom
      => FromSymbol (ExprSymbol atom t) where fromSymbol = wrap' ; {-# INLINE fromSymbol #-}




-- === ValidateLayout === ---
-- | Layout validation. Type-assertion utility, proving that symbol construction is not ill-typed.

type InvalidFormat sel a format = 'ShowType sel
                             :</>: Ticked ('ShowType a)
                             :</>: 'Text  "is not a valid"
                             :</>: Ticked ('ShowType format)


class                                                       ValidateScope scope sel a
instance {-# OVERLAPPABLE #-} ValidateScope_ scope sel a => ValidateScope scope sel a
instance {-# OVERLAPPABLE #-}                               ValidateScope I     sel a
instance {-# OVERLAPPABLE #-}                               ValidateScope scope I   a
instance {-# OVERLAPPABLE #-}                               ValidateScope scope sel I
type ValidateScope_ scope sel a = Assert (a `In` Atoms scope) (InvalidFormat sel a scope)


class                                                        ValidateLayout model sel a
instance {-# OVERLAPPABLE #-} ValidateLayout_ model sel a => ValidateLayout model sel a
instance {-# OVERLAPPABLE #-}                                ValidateLayout I     sel a
instance {-# OVERLAPPABLE #-}                                ValidateLayout model I   a
instance {-# OVERLAPPABLE #-}                                ValidateLayout model sel I
type ValidateLayout_ model sel a = ValidateScope (model ^. sel) sel a
type ValidateLayout' t     sel a = ValidateLayout (t ^. Layout) sel a

-- TODO: Booster Extension vvv
-- alias ValidateScope scope sel a = Assert (a `In` Atoms scope) (InvalidFormat sel a scope)
-- alias ValidateLayout model sel a = ValidateScope (model ^. sel) sel a




class SymbolEncoder atom where
    encodeSymbol :: forall t. ExprSymbol atom t -> TermStore


instance EncodeStore TermStoreSlots (HiddenSymbol atom) Identity
      => SymbolEncoder atom where
    encodeSymbol = runIdentity . encodeStore . hideLayout . unwrap' -- magic




type UncheckedTermCons atom m layers = (LayersCons layers m, SymbolEncoder atom)
type TermCons         atom m layers model = (UncheckedTermCons atom m layers, ValidateLayout model Atom atom)

-- | The `term` type does not force the construction to be checked,
--   because it has to be already performed in order to deliver ExprSymbol.
-- term :: UncheckedTermCons atom m t => ExprSymbol atom t -> m (Term3 t)
-- term a = flip Term3 (encodeSymbol a) <$> buildLayers


-- | The `expr` type does not force the construction to be checked,
--   because it has to be already performed in order to deliver ExprSymbol.
expr :: (expr ~ Expr t layers model, UncheckedTermCons atom m layers) => ExprSymbol atom expr -> m expr
expr a = flip Expr (encodeSymbol a) <$> buildLayers







-------------------------------------
-- === Expr Layout type caches === --
-------------------------------------

-- TODO: Refactor to Possible type class and arguments Variants etc.
-- type PossibleElements = [Static, Dynamic, Literal, Value, Thunk, Phrase, Draft, Acc, App, Blank, Cons, Curry, Lam, Match, Missing, Native, Star, Unify, Var]
type OffsetVariants = 7

-- type instance Encode rec (Symbol atom dyn a) = {-dyn-} 0 ': Decode rec atom ': {-formats-} '[6]


type instance Decode rec Static  = 0
type instance Decode rec Dynamic = 1

type instance Decode rec Literal = 2
type instance Decode rec Value   = 3
type instance Decode rec Thunk   = 4
type instance Decode rec Phrase  = 5
type instance Decode rec Draft   = 6

type instance Decode rec Acc     = 7
type instance Decode rec App     = 8
type instance Decode rec Blank   = 9
type instance Decode rec Cons    = 10
type instance Decode rec Curry   = 11
type instance Decode rec Lam     = 12
type instance Decode rec Match   = 13
type instance Decode rec Missing = 14
type instance Decode rec Native  = 15
type instance Decode rec Star    = 16
type instance Decode rec Unify   = 17
type instance Decode rec Var     = 18


type instance Encode2 Atom    v = Index v PossibleVariants
type instance Encode2 Format  v = Index v PossibleFormats


-- TODO: refactor, Decode2 should replace Decode. Refactor Decode -> Test
-- TODO: Decode2 should have more params to be more generic
type family Decode2 (ns :: [Nat]) :: [*] where
    Decode2 '[] = '[]
    Decode2 (n ': ns) = DecodeComponent n ': Decode2 ns

type family DecodeComponent (n :: Nat) :: *

type instance DecodeComponent 0 = Static
type instance DecodeComponent 1 = Dynamic

type instance DecodeComponent 2 = Literal
type instance DecodeComponent 3 = Value
type instance DecodeComponent 4 = Thunk
type instance DecodeComponent 5 = Phrase
type instance DecodeComponent 6 = Draft

type instance DecodeComponent 7  = Acc
type instance DecodeComponent 8  = App
type instance DecodeComponent 9  = Blank
type instance DecodeComponent 10 = Cons
type instance DecodeComponent 11 = Curry
type instance DecodeComponent 12 = Lam
type instance DecodeComponent 13 = Match
type instance DecodeComponent 14 = Missing
type instance DecodeComponent 15 = Native
type instance DecodeComponent 16 = Star
type instance DecodeComponent 17 = Unify
type instance DecodeComponent 18 = Var


type family All a :: [*]
type instance All Atom = '[Acc, App, Blank, Cons, Curry, Lam, Match, Missing, Native, Star, Unify, Var]
-- type instance All Atom = '[Acc, App]

-- class RecordRepr rec where
--     recordRepr :: rec -> String
--
-- instance RecordRepr

-- class Cons2 v t where
--     cons2 :: v -> t




-- === Expressions === --

-- star :: (LayersCons layers m, ValidateLayout model Atom Star) => m (Expr t layers model)
-- star = expr N.star'
