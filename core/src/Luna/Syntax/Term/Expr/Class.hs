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


-- === Utils === --

type SubLink    c t = Link t  (Sub c t)
type SubBinding c t = Binding (Sub c t)


-- === Instances === --

deriving instance Show (Unwrapped (Binding     tgt)) => Show (Binding     tgt)
deriving instance Show (Unwrapped (Link    src tgt)) => Show (Link    src tgt)

type instance Deconstructed (Binding a) = a
type instance Get p   (Binding a) = Get p a
type instance Set p v (Binding a) = Binding (Set p v a)





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

class Monad m => Constructor a m t where
    cons :: a -> m t

-- === Definition === --

data Stack layers (t :: ★ -> ★) where
    SLayer :: t l -> Stack ls t -> Stack (l ': ls) t
    SNull  :: Stack '[] t


-- === Construction === --

-- class    Monad m                               => StackCons t ls        m where newStack :: m (Stack ls t)
-- instance Monad m                               => StackCons t '[]       m where newStack = return SNull
-- instance (Generator m (t l), StackCons t ls m) => StackCons t (l ': ls) m where newStack = SLayer <$> new <*> newStack

instance ( Constructor a m (t l)
         , Constructor a m (Stack ls t)) => Constructor a m (Stack (l ': ls) t) where cons a = SLayer <$> cons a <*> cons a
instance Monad m                         => Constructor a m (Stack '[]       t) where cons _ = return SNull

-- instance

-- === Instances === --

instance Show (Stack '[] t) where
    show _ = ")"

instance (Show (t l), Show (Stack ls t)) => Show (Stack (l ': ls) t) where
    show (SLayer l ls) = show l <> ", " <> show ls



type instance Get p (Stack ls t) = t p

instance {-# OVERLAPPABLE #-}                          Getter p (Stack (p ': ls) t) where get (SLayer t _) = t
instance {-# OVERLAPPABLE #-} Getter p (Stack ls t) => Getter p (Stack (l ': ls) t) where get (SLayer _ l) = get @p l


------------------
-- === Expr === --
------------------

-- === Definitions === --

data Expr      t layers layout    = Expr (TermStack t layers layout) TermStore
type AnyExpr   t layers           = Expr t layers (Uniform Draft)
type PrimExpr  t layers name atom = Expr t layers (Prim name atom)
type PrimExpr' t layers      atom = PrimExpr t layers () atom

type TermStack t layers layout = Stack layers (Layer2 t layers layout)


-- === Properties === --

type instance Get Layout        (Expr _ _ layout) = layout
type instance Set Layout layout (Expr t layers _) = Expr t layers layout
type instance Get Data          (Expr _ _ _)      = TermStore
type instance Get TermType      (Expr t _ _)      = t
-- type instance Get Layers      (Expr _ layers _) = Proxy layers -- FIXME: when using ghc >= 8.0.1-head


instance Getter Data (Expr t layers layout) where get (Expr _ s) = s ; {-# INLINE get #-}


-- === Bindings === --

type instance Binder              (Expr t  _ _) = Binder   t
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




--------------------
-- === Layers === --
--------------------

-- === Definition === --

data family Layer2 t (layers :: [*]) layout l

class LayerCons2 l t layers m where
    consLayer2 :: forall layout. m (Layer2 t layers layout l)


-- === Parameters === --

-- data Layers

-- === Construction === --

type LayersCons2' t layers m = LayersCons2 layers t layers m
instance (LayersCons2 ls t layers m, LayerCons2 l t layers m)
                 => LayersCons2 (l ': ls) t layers m where buildLayers2 = SLayer <$> consLayer2 <*> buildLayers2
instance Monad m => LayersCons2 '[]       t layers m where buildLayers2 = return SNull
class    Monad m => LayersCons2 ls        t layers m where buildLayers2 :: forall layout. m (Stack ls (Layer2 t layers layout))


--------------------
-- === Layers === --
--------------------

-- === Definition === --

-- data family Layer  t l

class LayerCons3 l m t where
    consLayer3 :: m (Layer t l)


-- === Parameters === --

-- data Layers

-- === Construction === --

class    Monad m                           => LayersCons3 ls        t m where buildLayers3 :: m (Stack ls (Layer t))
instance Monad m                           => LayersCons3 '[]       t m where buildLayers3 = return SNull
instance (LayersCons3 ls t m, LayerCons m l) => LayersCons3 (l ': ls) t m where buildLayers3 = SLayer <$> consLayer <*> buildLayers3











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


-- moze powinnismy zrobic prostsze expression ktore towrylyby sie w layoucie top (latwo konwertowalnym do wszystkiego innego?)

class SymbolEncoder atom where
    encodeSymbol :: forall t. ExprSymbol atom t -> TermStore


instance EncodeStore TermStoreSlots (HiddenSymbol atom) Identity
      => SymbolEncoder atom where
    encodeSymbol = runIdentity . encodeStore . hideLayout . unwrap' -- magic




type UncheckedTermCons atom t layers m = (LayersCons2' t layers m, SymbolEncoder atom)
type TermCons         atom t layers layout m = (UncheckedTermCons atom t layers m, ValidateLayout layout Atom atom)

-- | The `term` type does not force the construction to be checked,
--   because it has to be already performed in order to deliver ExprSymbol.
-- term :: UncheckedTermCons atom m t => ExprSymbol atom t -> m (Term3 t)
-- term a = flip Term3 (encodeSymbol a) <$> buildLayers


-- | The `expr` type does not force the construction to be checked,
--   because it has to be already performed in order to deliver ExprSymbol.
expr :: (expr ~ Expr t layers layout, UncheckedTermCons atom t layers m) => ExprSymbol atom expr -> m expr
expr a = flip Expr (encodeSymbol a) <$> buildLayers2






------- Data Layer

type family ExprLayer l t
newtype Layer4 t l = Layer4 (ExprLayer l t)

type instance ExprLayer Data t = TermStore

instance Monad m => Constructor TermStore m (Layer4 t Data) where cons = return . Layer4

-- instance Monad m => Constructor TermStore m (ExprLayer Data t) where
--     cons = return

deriving instance Show (ExprLayer l t) => Show (Layer4 t l)

makeWrapped ''Layer4

--------------------
-- === Layers === --
--------------------

-- === Definition === --

-- data family Layer4 t l



------------------
-- === Expr === --
------------------

type TermStack2 t layers layout = Stack layers (Layer4 (Expr2 t layers layout))


-- === Classes === --

class HasLayer layer layers where
    layer :: forall t layout layers'. Stack layers (Layer4 (Expr2 t layers' layout)) -> ExprLayer layer (Expr2 t layers' layout)

instance {-# OVERLAPPABLE #-}                  HasLayer l (l ': ls) where layer (SLayer t _) = unwrap' t
instance {-# OVERLAPPABLE #-} HasLayer l ls => HasLayer l (t ': ls) where layer (SLayer _ s) = layer @l s


-- === Definitions === --

newtype Expr2  t layers layout    = Expr2 (TermStack2 t layers layout)

makeWrapped ''Expr2
-- type AnyExpr   t layers           = Expr t layers (Uniform Draft)
type PrimExpr2  t layers name atom = Expr2 t layers (Prim name atom)
type PrimExpr2' t layers      atom = PrimExpr2 t layers () atom
--
--
--
-- -- === Properties === --
--
type instance Get Layout        (Expr2 _ _ layout) = layout
-- type instance Set Layout layout (Expr t layers _) = Expr t layers layout
type instance Get Data          (Expr2 t layers layout) = Unwrapped (Get Data (Unwrapped (Expr2 t layers layout))) -- TODO: simplify
-- type instance Get TermType      (Expr t _ _)      = t
-- -- type instance Get Layers      (Expr _ layers _) = Proxy layers -- FIXME: when using ghc >= 8.0.1-head
--
--
instance HasLayer Data layers => Getter Data (Expr2 t layers layout) where get = layer @Data . unwrap' ; {-# INLINE get #-}
--


-- === Bindings === --

type instance Binder               (Expr2 t  _ _) = Binder   t
type instance Linker (Expr2 t _ _) (Expr2 t' _ _) = Linker t t'

instance Bindable t m => Bindable (Expr2 t layers model) m where
    mkBinder    = mkBinder    @t ; {-# INLINE mkBinder    #-}
    rmBinder    = rmBinder    @t ; {-# INLINE rmBinder    #-}
    writeBinder = writeBinder @t ; {-# INLINE writeBinder #-}
    readBinder  = readBinder  @t ; {-# INLINE readBinder  #-}

instance Linkable t t' m => Linkable (Expr2 t layers model) (Expr2 t' layers' model') m where
    mkLinker    = mkLinker    @t @t' ; {-# INLINE mkLinker    #-}
    rmLinker    = rmLinker    @t @t' ; {-# INLINE rmLinker    #-}
    writeLinker = writeLinker @t @t' ; {-# INLINE writeLinker #-}
    readLinker  = readLinker  @t @t' ; {-# INLINE readLinker  #-}


-- === Instances === --

type instance Sub s (Expr2 t layers layout) = Expr2 t layers (Sub s layout)




data AnyLayout = AnyLayout deriving (Show)

type instance Sub t AnyLayout = AnyLayout

-- TODO: zamiast zahardcodoewanego `a` mozna uzywac polimorficznego, ktorego poszczegolne komponenty jak @Data bylyby wymuszane przez poszczegolne warstwy
-- expr2 :: (Constructor a m (TermStack2 t layers layout), expr ~ Expr2 t layers layout, a ~ ExprSymbol atom expr) => a -> m expr
-- expr2 a = Expr2 <$> cons a

expr3 :: (SymbolEncoder atom, Constructor TermStore m (TermStack2 t layers AnyLayout), expr ~ Expr2 t layers layout) => ExprSymbol atom expr -> m expr
expr3 a = specifyLayout . Expr2 <$> cons (encodeSymbol a)

specifyLayout :: Expr2 t layers AnyLayout -> Expr2 t layers layout
specifyLayout = unsafeCoerce

specifyLayout2 :: Binding (Expr2 t layers AnyLayout) -> Binding (Expr2 t layers layout)
specifyLayout2 = unsafeCoerce

anyLayout :: Expr2 t layers layout -> Expr2 t layers AnyLayout
anyLayout = unsafeCoerce

anyLayout2 :: Binding (Expr2 t layers layout) -> Binding (Expr2 t layers AnyLayout)
anyLayout2 = unsafeCoerce

-- === Instances === --

deriving instance Show (Unwrapped (Expr2 t layers layout)) => Show (Expr2 t layers layout)
-- deriving as Unwrapped instance Show (Expr2 t layers layout)





-- class Monad m => Generator     m a where new         :: m a                      ; default new :: Maker a => Deconstructed a -> m a










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
