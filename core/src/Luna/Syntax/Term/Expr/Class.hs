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
import Data.Graph.Model.Edge (Edge) -- Should be removed as too deep dependency?
data {-kind-} Layout dyn form = Layout dyn form deriving (Show)

type instance Get Dynamics (Layout dyn form) = dyn
type instance Get Format   (Layout dyn form) = form




data a := b

type instance Get t (l  := v ': ls) = If (t == l) v (Get t ls)
type instance Get t (l ':= v ': ls) = If (t == l) v (Get t ls)



type PossibleVariants = [Acc, App, Blank, Cons, Curry, Lam, Match, Missing, Native, Star, Unify, Var]
type PossibleFormats  = [Literal, Value, Thunk, Phrase, Draft]





-- === Properties === --

-- TODO: refactor
data Name   = Name   deriving (Show)
data Model  = Model  deriving (Show)
data Data   = Data   deriving (Show)
data System = System deriving (Show)








-- ##################################################################################
-- ##################################################################################
-- data Network2 = Network2 deriving (Show)
-- data NetworkT a = NetworkT a deriving (Show)
-- type instance Fields Network2 = '[]
-- type instance Dict   Network2 = '[]
--
-- type instance Fields (NetworkT a) = Fields a
-- type instance Dict   (NetworkT a) = Dict   a
-- ---------------------
-- -- === Records === --
-- ---------------------
--
-- -- === Properties === --
--
-- type family Dict   t :: [Assoc * *]
-- type family Fields t :: [*]
--
-- type family Field    field  t
-- type family MapField fields t where
--     MapField '[]       t = '[]
--     MapField (f ': fs) t = Field f t ': MapField fs t
--
-- newtype Record t = Record (Unwrapped (Record t))
--
--
-- -- === Instances === --
--
-- -- Wrapper
-- -- makeWrapped ''Record -- GHC8 TH cannot represent kind casts
-- instance Wrapped   (Record t) where
--     type Unwrapped (Record t) = TMap (Fields t :=: MapField (Fields t) t)
--     _Wrapped' = iso (\(Record t) -> t) Record ; {-# INLINE _Wrapped' #-}
--
-- -- Show
-- instance Show (Unwrapped (Unwrapped (Record t))) => Show (Record t) where
--     showsPrec d (Record t) = showParen (d > app_prec) $
--         showString "Record " . showsPrec (app_prec+1) (unwrap t)
--         where app_prec = 10
--
--
-- -- Property access
-- type instance Get p (Record t) = Get p (Unwrapped (Record t))
--
-- instance Getter p (Unwrapped (Record t))
--       => Getter p (Record t) where get = Prop.get @p . unwrap' ; {-# INLINE get #-}
-- ##################################################################################
-- ##################################################################################



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

data Stack3 layers (t :: ★ -> ★) where
    SLayer3 :: t l -> Stack3 ls t -> Stack3 (l ': ls) t
    SNull3  :: Stack3 '[] t


-- === Instances === --

instance Show (Stack3 '[] t) where
    show _ = ")"

instance (Show (t l), Show (Stack3 ls t)) => Show (Stack3 (l ': ls) t) where
    show (SLayer3 l ls) = show l <> ", " <> show ls



------------------
-- === Term === --
------------------

-- === Definitions === --

data Term t (layers :: [*]) model = Term (TermStack t layers model) TermStore

type TermStack t layers model = Stack3 layers (Layer (Term t layers model))


-- === Properties === --

type instance Get Model       (Term _ _ model)  = model
type instance Set Model model (Term t layers _) = Term t layers model

type instance Get Data        (Term _ _ _) = TermStore

instance Getter Data (Term t layers model) where get (Term _ s) = s ; {-# INLINE get #-}


-- === Instances === --

type instance Connector (Term t _ _) = Connector t
type instance Sub s (Term t layers model) = Term t layers (Sub s model)

deriving instance Show (TermStack t layers model) => Show (Term t layers model)



--------------------
-- === Layers === --
--------------------

-- === Definition === --

data family Layer  t l
type family Layers a :: [*]

class LayerCons m l where
    consLayer :: forall t. m (Layer t l)


-- === Construction === --

class    Monad m                          => LayersCons ls        m where buildLayers :: forall t. m (Stack3 ls (Layer t))
instance Monad m                          => LayersCons '[]       m where buildLayers = return SNull3
instance (LayersCons ls m, LayerCons m l) => LayersCons (l ': ls) m where buildLayers = SLayer3 <$> consLayer <*> buildLayers



----------------------
-- === Bindings === --
----------------------

-- === Definition === --

data family Binding b a
newtype instance Binding b (Term t layers model) = Binding (Connector t b (Term t layers model))

-- | Param `t` defines the Connector type. The args are connection type and connection target.
--   For example `Connector Net Node (Term ...)`
type family Connector t :: * -> * -> *


-- === Utils === --

type Connection c t = Binding Edge (Sub c t)


-- === Instances === --

type instance Deconstructed (Binding b a) = a
type instance Get p   (Binding b a) = Get p a
type instance Set p v (Binding b a) = Binding b (Set p v a)











newtype TermSymbol atom t = TermSymbol (N.NamedSymbol atom (Connection Name t) (Connection Atom t))
makeWrapped ''TermSymbol

type instance Get p (TermSymbol atom t) = Get p (Unwrapped (TermSymbol atom t))


instance ValidateModel (Get Model t) Atom atom
      => FromSymbol (TermSymbol atom t) where fromSymbol = wrap' ; {-# INLINE fromSymbol #-}




-- === ValidateModel === ---
-- | Model validation. Type-assertion utility, proving that symbol construction is not ill-typed.

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


class                                                       ValidateModel model sel a
instance {-# OVERLAPPABLE #-} ValidateModel_ model sel a => ValidateModel model sel a
instance {-# OVERLAPPABLE #-}                               ValidateModel I     sel a
instance {-# OVERLAPPABLE #-}                               ValidateModel model I   a
instance {-# OVERLAPPABLE #-}                               ValidateModel model sel I
type ValidateModel_ model sel a = ValidateScope (model ^. sel) sel a
type ValidateModel' t     sel a = ValidateModel (t ^. Model) sel a

-- TODO: Booster Extension vvv
-- alias ValidateScope scope sel a = Assert (a `In` Atoms scope) (InvalidFormat sel a scope)
-- alias ValidateModel model sel a = ValidateScope (model ^. sel) sel a




class SymbolEncoder atom where
    encodeSymbol :: forall t. TermSymbol atom t -> TermStore


instance EncodeStore TermStoreSlots (HiddenSymbol atom) Identity
      => SymbolEncoder atom where
    encodeSymbol = runIdentity . encodeStore . hideLayout . unwrap' -- magic




type UncheckedTermCons atom m layers = (LayersCons layers m, SymbolEncoder atom)
type TermCons         atom m layers model = (UncheckedTermCons atom m layers, ValidateModel model Atom atom)

-- | The `term` type does not force the construction to be checked,
--   because it has to be already performed in order to deliver TermSymbol.
-- term :: UncheckedTermCons atom m t => TermSymbol atom t -> m (Term3 t)
-- term a = flip Term3 (encodeSymbol a) <$> buildLayers


-- | The `term` type does not force the construction to be checked,
--   because it has to be already performed in order to deliver TermSymbol.
term2 :: (term ~ Term t layers model, UncheckedTermCons atom m layers) => TermSymbol atom term -> m term
term2 a = flip Term (encodeSymbol a) <$> buildLayers







-------------------------------------
-- === Term Layout type caches === --
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
