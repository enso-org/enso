{-# LANGUAGE CPP                    #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE GADTs #-}




module Luna.Syntax.Term.Expr.Class where


import           Prelude.Luna                 hiding (Field2, Enum, Num, Swapped, Curry, String, Integer, Rational, Symbol, Index, Data, Field)
import qualified Prelude.Luna                 as P

import           Data.Abstract
import           Data.Base
import           Data.Record                  hiding (Layout, Variants, Match, Cons, Value, cons)
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
-- import Control.Monad.State
import Control.Lens.Property hiding (Constructor)
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


type ContentShow a = Show (Content a)
newtype Content a = Content a deriving (Functor, Traversable, Foldable)
makeWrapped ''Content

contentShow :: ContentShow a => a -> P.String
contentShow = show . Content


class Monad m => Constructor a m t where
    cons :: a -> m t


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




data AnyLayout = AnyLayout deriving (Show)
type instance Get Atom AnyLayout = AnyLayout
type instance Atoms AnyLayout = '[Star]

type instance Sub t AnyLayout = AnyLayout

-- TODO: zamiast zahardcodoewanego `a` mozna uzywac polimorficznego, ktorego poszczegolne komponenty jak @Data bylyby wymuszane przez poszczegolne warstwy
-- expr2 :: (Constructor a m (ExprStack t layers layout), expr ~ Expr t layers layout, a ~ ExprSymbol atom expr) => a -> m expr
-- expr2 a = Expr <$> cons a

-- | The `expr` type does not force the construction to be checked,
--   because it has to be already performed in order to deliver ExprSymbol.








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



-------------------------
-- === Connections === --
-------------------------

-- === Definitions === --

type family Binder    t  :: * -> *
type family Linker  t t' :: * -> * -> *
type        Binder'     tgt = Binder     tgt     tgt
type        Linker' src tgt = Linker src tgt src tgt

newtype Binding     tgt = Binding (Binder' tgt)
newtype Link    src tgt = Link    (Linker' src tgt)
type    Link'   a       = Link    a a
makeWrapped ''Binding
makeWrapped ''Link


-- === Classes === --

class Monad m => Bindable t m where
    mkBinder    :: forall a. a -> m (Binder t a)
    rmBinder    :: forall a. Binder t a      -> m ()
    writeBinder :: forall a. Binder t a -> a -> m ()
    readBinder  :: forall a. Binder t a      -> m a

mkBinding    :: forall a m. Bindable a m => a -> m (Binding a)
rmBinding    :: forall a m. Bindable a m => Binding a      -> m ()
writeBinding :: forall a m. Bindable a m => Binding a -> a -> m ()
readBinding  :: forall a m. Bindable a m => Binding a      -> m a
mkBinding    = wrap' <∘> mkBinder @a    ; {-# INLINE mkBinding    #-}
rmBinding    = rmBinder    @a . unwrap' ; {-# INLINE rmBinding    #-}
writeBinding = writeBinder @a . unwrap' ; {-# INLINE writeBinding #-}
readBinding  = readBinder  @a . unwrap' ; {-# INLINE readBinding  #-}


type Linkable' t m = Linkable t t m
class Monad m => Linkable t t' m where
    mkLinker    :: forall a b. Binding a -> Binding b -> m (Linker t t' a b)
    rmLinker    :: forall a b. Linker t t' a b -> m ()
    writeLinker :: forall a b. Linker t t' a b -> Binding a -> Binding b -> m ()
    readLinker  :: forall a b. Linker t t' a b -> m (Binding a, Binding b)

mkLink    :: forall a b m. Linkable a b m => Binding a -> Binding b -> m (Link a b)
rmLink    :: forall a b m. Linkable a b m => Link a b -> m ()
writeLink :: forall a b m. Linkable a b m => Link a b -> Binding a -> Binding b -> m ()
readLink  :: forall a b m. Linkable a b m => Link a b -> m (Binding a, Binding b)
mkLink    = wrap' <∘∘> mkLinker @a @b   ; {-# INLINE mkLink    #-}
rmLink    = rmLinker    @a @b . unwrap' ; {-# INLINE rmLink    #-}
writeLink = writeLinker @a @b . unwrap' ; {-# INLINE writeLink #-}
readLink  = readLinker  @a @b . unwrap' ; {-# INLINE readLink  #-}


type family Content2 a

type family Result (m :: * -> *) where
    Result ((->) t) = t
    Result m        = ()

type NoResult m = Result m ~ ()


class Connection a m where
    read     :: a -> m (Content2 a)
    write    :: a -> Content2 a -> m (Result m)

class XBuilder a m where
    bindings :: m [Binding a]
    -- links    :: m [Link'   a]


type instance Content2 (Binding a)   = a
type instance Content2 (Link    a b) = (Binding a, Binding b)

instance {-# OVERLAPPABLE #-} (Bindable a m, NoResult m) => Connection (Binding a) m where
    read  = readBinding  ; {-# INLINE read  #-}
    write = writeBinding ; {-# INLINE write #-}




-- === Utils === --

type SubLink    c t = Link t  (Sub c t)
type SubBinding c t = Binding (Sub c t)


-- === Instances === --

deriving instance Show (Unwrapped (Binding     tgt)) => Show (Binding     tgt)
deriving instance Show (Unwrapped (Link    src tgt)) => Show (Link    src tgt)

-- type instance Get p   (Binding a) = Get p a
-- type instance Set p v (Binding a) = Binding (Set p v a)



------------------------
-- === ExprSymbol === --
------------------------

newtype ExprSymbol atom t = ExprSymbol (N.NamedSymbol atom (SubLink Name t) (SubLink Atom t))
makeWrapped ''ExprSymbol

-- === Instances === --

type instance Get p (ExprSymbol atom t) = Get p (Unwrapped (ExprSymbol atom t))
instance ValidateLayout (Get Layout t) Atom atom
      => FromSymbol (ExprSymbol atom t) where fromSymbol = wrap' ; {-# INLINE fromSymbol #-}



----------------------
-- === TermData === --
----------------------

type TermStoreSlots = '[ Atom ':= Enum, Format ':= Mask, Sym ':= Raw ]
type TermStore = Store2 TermStoreSlots

newtype TermData sys model = TermData TermStore deriving (Show)
makeWrapped ''TermData


-- === Encoding === --

class                                                               SymbolEncoder atom where encodeSymbol :: forall t. ExprSymbol atom t -> TermStore
instance                                                            SymbolEncoder I    where encodeSymbol = impossible
instance EncodeStore TermStoreSlots (HiddenSymbol atom) Identity => SymbolEncoder atom where
    encodeSymbol = runIdentity . encodeStore . hideLayout . unwrap' ; {-# INLINE encodeSymbol #-} -- magic



-------------------
-- === Stack === --
-------------------

data Stack layers (t :: ★ -> ★) where
    SLayer :: t l -> Stack ls t -> Stack (l ': ls) t
    SNull  :: Stack '[] t


-- === Instances === --

-- Show
instance ContentShow (Stack ls t)               => Show          (Stack ls        t)  where show s                        = "(" <> contentShow s <> ")"      ; {-# INLINE show #-}
instance                                           Show (Content (Stack '[]       t)) where show _                        = ""                               ; {-# INLINE show #-}
instance (Show (t l), ContentShow (Stack ls t)) => Show (Content (Stack (l ': ls) t)) where show (unwrap' -> SLayer l ls) = show l <> ", " <> contentShow ls ; {-# INLINE show #-}
instance {-# OVERLAPPING #-} Show (t l)         => Show (Content (Stack '[l]      t)) where show (unwrap' -> SLayer l ls) = show l                           ; {-# INLINE show #-}

-- Constructor
instance ( Constructor a m (t l)
         , Constructor a m (Stack ls t)) => Constructor a m (Stack (l ': ls) t) where cons a = SLayer <$> cons a <*> cons a ; {-# INLINE cons #-}
instance Monad m                         => Constructor a m (Stack '[]       t) where cons _ = return SNull                 ; {-# INLINE cons #-}


-- Properties
type instance Get p (Stack ls t) = t p

instance {-# OVERLAPPABLE #-}                          Getter p (Stack (p ': ls) t) where get (SLayer t _) = t        ; {-# INLINE get #-}
instance {-# OVERLAPPABLE #-} Getter p (Stack ls t) => Getter p (Stack (l ': ls) t) where get (SLayer _ l) = get @p l ; {-# INLINE get #-}



--------------------
-- === Layers === --
--------------------

type family ExprLayer l t
newtype Layer t l = Layer (ExprLayer l t)

type instance ExprLayer Data t = TermStore

instance Monad m => Constructor TermStore m (Layer t Data) where cons = return . Layer



deriving instance Show (ExprLayer l t) => Show (Layer t l)

makeWrapped ''Layer

-- === Classes === --

class HasLayer layer layers where
    layer :: forall t layout layers'. Stack layers (Layer (Expr t layers' layout)) -> ExprLayer layer (Expr t layers' layout)

instance {-# OVERLAPPABLE #-}                  HasLayer l (l ': ls) where layer (SLayer t _) = unwrap' t
instance {-# OVERLAPPABLE #-} HasLayer l ls => HasLayer l (t ': ls) where layer (SLayer _ s) = layer @l s



------------------
-- === Expr === --
------------------

-- === Definitions === --

newtype Expr    t layers layout = Expr (ExprStack t layers layout)
type    AnyExpr t layers        = Expr t layers AnyLayout

type ExprStack    t layers layout = Stack layers (Layer (Expr t layers layout))
type AnyExprStack t layers        = Stack layers (Layer (Expr t layers AnyLayout))


-- === Utils === --

expr :: (SymbolEncoder atom, Constructor TermStore m (AnyExprStack t layers), expr ~ Expr t layers layout) => ExprSymbol atom expr -> m expr
expr a = specifyLayout . Expr <$> cons (encodeSymbol a)

-- TODO: refactor vvv
specifyLayout :: AnyExpr t layers -> Expr t layers layout
specifyLayout = unsafeCoerce


-- === Properties === --

type instance Get Layout        (Expr _ _ layout) = layout
type instance Set Layout layout (Expr t layers _) = Expr t layers layout
type instance Get Data          (Expr t layers layout) = Unwrapped (Get Data (Unwrapped (Expr t layers layout))) -- TODO: simplify


-- === Bindings === --

type instance Binder              (Expr t  _ _) = Binder t
type instance Linker (Expr t _ _) (Expr t' _ _) = Linker t t'

instance Bindable t m => Bindable (Expr t layers model) m where
    mkBinder    = mkBinder    @t ; {-# INLINE mkBinder    #-}
    rmBinder    = rmBinder    @t ; {-# INLINE rmBinder    #-}
    writeBinder = writeBinder @t ; {-# INLINE writeBinder #-}
    readBinder  = readBinder  @t ; {-# INLINE readBinder  #-}

instance Linkable t t' m => Linkable (Expr t layers model) (Expr t' layers' model') m where
    mkLinker    = mkLinker    @t @t' ; {-# INLINE mkLinker    #-}
    rmLinker    = rmLinker    @t @t' ; {-# INLINE rmLinker    #-}
    writeLinker = writeLinker @t @t' ; {-# INLINE writeLinker #-}
    readLinker  = readLinker  @t @t' ; {-# INLINE readLinker  #-}


-- === Instances === --

-- Wrapper
makeWrapped ''Expr

-- Show
deriving instance Show (Unwrapped (Expr t layers layout)) => Show (Expr t layers layout)

-- Sub
type instance Sub s (Expr t layers layout) = Expr t layers (Sub s layout)

-- Layers
instance HasLayer Data layers => Getter Data (Expr t layers layout) where get = layer @Data . unwrap' ; {-# INLINE get #-}




------------------------- something




specifyLayout2 :: Binding (Expr t layers AnyLayout) -> Binding (Expr t layers layout)
specifyLayout2 = unsafeCoerce

anyLayout :: Expr t layers layout -> Expr t layers AnyLayout
anyLayout = unsafeCoerce

anyLayout2 :: Binding (Expr t layers layout) -> Binding (Expr t layers AnyLayout)
anyLayout2 = unsafeCoerce








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
