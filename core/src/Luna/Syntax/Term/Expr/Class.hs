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

data {-kind-} Layout dyn form = Layout dyn form deriving (Show)

type instance Get Dynamics (Layout dyn form) = dyn
type instance Get Format   (Layout dyn form) = form


data Model    = Model   deriving (Show)
-- data Binding  = Binding deriving (Show)
data SubModel = SubModel   deriving (Show)

type family Bound layout (attrs :: [*]) :: * -- to musi byc tak zadeklarowane, poniewaz w ``(Binding (term # Layout) term)`, `term` moze byc zbyt rozpakowanego typu



data a := b

type instance Get t (l  := v ': ls) = If (t == l) v (Get t ls)
type instance Get t (l ':= v ': ls) = If (t == l) v (Get t ls)




data EE = EE -- TODO: refactor to Expr selector

type PossibleVariants = [Acc, App, Blank, Cons, Curry, Lam, Match, Missing, Native, Star, Unify, Var]
type PossibleFormats  = [Literal, Value, Thunk, Phrase, Draft]





-- === Properties === --

-- TODO: refactor
data Data = Data deriving (Show)
data System = System deriving (Show)





data Network2 = Network2 deriving (Show)
data NetworkT a = NetworkT a deriving (Show)
type instance Fields Network2 = '[]
type instance Dict   Network2 = '[]

type instance Fields (NetworkT a) = Fields a
type instance Dict   (NetworkT a) = Dict   a




---------------------
-- === Records === --
---------------------

-- === Properties === --

type family Dict   t :: [Assoc * *]
type family Fields t :: [*]

type family Field    field  t
type family MapField fields t where
    MapField '[]       t = '[]
    MapField (f ': fs) t = Field f t ': MapField fs t

newtype Record t = Record (Unwrapped (Record t))


-- === Instances === --

-- Wrapper
-- makeWrapped ''Record -- GHC8 TH cannot represent kind casts
instance Wrapped   (Record t) where
    type Unwrapped (Record t) = TMap (Fields t :=: MapField (Fields t) t)
    _Wrapped' = iso (\(Record t) -> t) Record ; {-# INLINE _Wrapped' #-}

-- Show
instance Show (Unwrapped (Unwrapped (Record t))) => Show (Record t) where
    showsPrec d (Record t) = showParen (d > app_prec) $
        showString "Record " . showsPrec (app_prec+1) (unwrap t)
        where app_prec = 10


-- Property access
type instance Get p (Record t) = Get p (Unwrapped (Record t))

instance Getter p (Unwrapped (Record t))
      => Getter p (Record t) where get = Prop.get @p . unwrap' ; {-# INLINE get #-}

--
-- -------------------
-- -- === Term2 === --
-- -------------------
--
-- -- === Properties === --
--
-- -- type family Dict   t :: [Assoc * *]
-- -- type family Fields t :: [*]
--
-- type family Field2    field  t :: * -> *
-- type family MapField2 fields t model where
--     MapField2 '[]       t model = '[]
--     MapField2 (f ': fs) t model = Field2 f t model ': MapField2 fs t model
--
--
-- newtype Term2 t model = Term2 (Unwrapped (Term2 t model))
--
--
-- -- === Instances === --
--
-- -- Wrapper
-- -- makeWrapped ''Record -- GHC8 TH cannot represent kind casts
-- instance Wrapped   (Term2 t model) where
--     type Unwrapped (Term2 t model) = TMap (Fields t :=: MapField2 (Fields t) t model)
--     _Wrapped' = iso (\(Term2 t) -> t) Term2 ; {-# INLINE _Wrapped' #-}
-- --
-- -- -- Show
-- -- instance Show (Unwrapped (Unwrapped (Record t))) => Show (Record t) where
-- --     showsPrec d (Record t) = showParen (d > app_prec) $
-- --         showString "Record " . showsPrec (app_prec+1) (unwrap t)
-- --         where app_prec = 10
-- --
-- --
-- -- -- Property access
-- -- type instance Get p (Record t) = Get p (Unwrapped (Record t))
-- --
-- -- instance Getter p (Unwrapped (Record t))
-- --       => Getter p (Record t) where get = Prop.get @p . unwrap' ; {-# INLINE get #-}
-- --
--
--
-- class ConsTermx a m t where
--     consTermx :: forall model. a -> m (Term t model)
--
--
--
-- --
--




------------------
-- === Term === --
------------------

-- === Definitions === --

data    TermDesc t model
newtype Term     t model = Term (Record (TermDesc t model))
makeWrapped ''Term


-- === Instances === --

-- Properties

type instance Fields (TermDesc t model) = Data ': Fields t
type instance Dict   (TermDesc t model) = System ':= t
                                       ': Model  ':= model
                                       ': Dict t


--
type instance Get p (Term     t model) = Get p (Unwrapped (Term t model))
type instance Get p (TermDesc t model) = Get p (Dict (TermDesc t model))

instance Getter p (Unwrapped (Term t model))
      => Getter p (Term t model) where get = Prop.get @p . unwrap' ; {-# INLINE get #-}

-- Show

deriving instance Show (Unwrapped (Term t model)) => Show (Term t model)



----------------------
-- === TermData === --
----------------------

-- === Definition === --

type TermStoreSlots = '[ Atom ':= Enum, Format ':= Mask, Sym ':= Raw ]
type TermStore = Store2 TermStoreSlots

newtype TermData sys model = TermData TermStore deriving (Show)
makeWrapped ''TermData


-- === Instances === --

type instance Field Data t = TermData (t ^. System) (t ^. Model)








-- class TermCons a m t where
--     termCons :: forall model. a -> m (Term t model)
--
-- instance (RecCons a m t, Functor m) => TermCons a m t where
--     termCons a = wrap' <$> recCons a
--
--
--
-- class RecCons a m t where
--     recCons :: forall model. a -> m (Record (TermDesc t model))
--
-- instance RecCons a m t where
--     recCons =
--
--
-- class FieldCons a m f t where
--     fieldCons :: forall model. a -> m (Field f (TermDesc t model))




    -- type Unwrapped (Record t) = TMap (Fields t :=: MapField (Fields t) t)



-------------------
-- === Stack === --
-------------------

-- === Definition === --

data Stack3 layers (t :: ★ -> ★) where
    SLayer3 :: t l -> Stack3 ls t -> Stack3 (l ': ls) t
    SNull3  :: Stack3 '[] t
--
-- -- === Utils === --
--
-- -- Construction
--

------------------
-- === Term === --
------------------

-- === Definitions === --

data Term3 t = Term3 (TermStack t) TermStore


-- === Term stack === --

type TermStack t = Stack3 (Layers t) (Layer t)

class    Monad m                          => LayersCons ls        m where buildLayers :: forall t. m (Stack3 ls (Layer t))
instance Monad m                          => LayersCons '[]       m where buildLayers = return SNull3
instance (LayersCons ls m, LayerCons m l) => LayersCons (l ': ls) m where buildLayers = SLayer3 <$> consLayer <*> buildLayers


-- === Layers === --

data family Layer  t l
type family Layers a :: [*]

class LayerCons m l where
    consLayer :: forall t. m (Layer t l)



data Name = Name

type family Binding t :: * -> *

-- type Connection c t = Binding t (Term3 (Sub c t))

type family Connection c t where
    -- Connection I t = I
    -- Connection c I = I
    Connection c t = Binding t (Term3 (Sub c t))

type Connection' c t = Term3 (Sub c t)


newtype TermSymbol atom t = TermSymbol (N.NamedSymbol atom (Connection Name t) (Connection Atom t))
makeWrapped ''TermSymbol

type instance Get p (TermSymbol atom t) = Get p (Unwrapped (TermSymbol atom t))



instance ValidateModel (t ^. Model) Atom atom
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




type UncheckedTermCons atom m t = (LayersCons (Layers t) m, SymbolEncoder atom)
type TermCons          atom m t = (UncheckedTermCons atom m t, ValidateModel (t ^. Model) Atom atom)

-- | The `term` type does not force the construction to be checked,
--   because it has to be already performed in order to deliver TermSymbol.
term :: UncheckedTermCons atom m t => TermSymbol atom t -> m (Term3 t)
term a = flip Term3 (encodeSymbol a) <$> buildLayers











-- type Term = Term (Network )

------------------
-- === Term === --
------------------

-- === Definitions === --

data Term2    t model = Term2 (Stack (Fields t) t model) TermStore

data family TermLayer l t model

data Stack layers t model where
    SLayer :: TermLayer l t model -> Stack ls t tmodel -> Stack (l ': ls) t model
    SNull  :: Stack '[] t model


-- === Classes === --

class ConsTermLayer m l t where
    consTermLayer :: m (TermLayer l t model)

type ASTBuilder t m = (ConsStack (Fields t) t m, Monad m)
class ConsStack ls t m where
    consStack :: forall model. m (Stack ls t model)

instance Monad m                                          => ConsStack '[]       t m where consStack = return SNull
instance (Monad m, ConsStack ls t m, ConsTermLayer m l t) => ConsStack (l ': ls) t m where consStack = SLayer <$> consTermLayer <*> consStack


-- class ConsTerm a m t where
--     consTerm :: forall model. a -> m (Term2 t model)

-- instance (ConsStack' t m, Cons2 a m TermStore) => ConsTerm a m t where
--     consTerm a = Term2 <$> consStack <*> cons2 a

type AtomBuilder a m = Cons2 a m TermStore

unsafeConsTerm :: ( ASTBuilder t m, AtomBuilder (Symbol atom layout) m
                  , layout ~ BindModel Sym t model, scope ~ Scope Atom model)
               => (Symbol atom layout) -> m (Term2 t model)
unsafeConsTerm a = Term2 <$> consStack <*> cons2 a

consTerm :: ( ASTBuilder t m, AtomBuilder (Symbol atom layout) m
            , layout ~ BindModel Sym t model, scope ~ Scope Atom model
            , Assert (atom `In` Atoms scope) (InvalidAtomFormat atom scope))
         => (Symbol atom layout) -> m (Term2 t model)
consTerm a = Term2 <$> consStack <*> cons2 a

type family MatchModel a t model :: Constraint

type instance MatchModel (Symbol atom layout) t model = ( -- layout ~ ModelLayout t model
                                                        layout ~ BindModel Sym t model
                                                        -- , scope  ~ Scope Atom model
                                                        , Assert (atom `In` Atoms (Scope Atom model)) (InvalidAtomFormat atom (Scope Atom model))
                                                        )




-- term :: ( ASTBuilder t m, AtomBuilder (Symbol atom (Model t model)) m, scope ~ Scope Atom model
--              , Assert (atom `In` Atoms scope) (InvalidAtomFormat atom scope
--              )
--           => Symbol atom (Model t model) -> m (Term2 t model)
-- term a = Term2 <$> consStack <*> cons2 a



type family   Subscope t model
type family   Scope    t model

type family   Bind t sys model
type family   Bind2 t sys model
type family   ModelLayout sys model


type instance Scope Atom (Layout.Named n a) = Scope Atom a

type instance ModelLayout sys (Layout.Named n a) = Layout.Named (Bind Name sys (Layout.Named n a))
                                                                (Bind Atom sys (Layout.Named n a))




type family BindModel t sys model

-- instance ConsTerm (Symbol atom layout) m t where



-- instance (Monad m, EncodeStore slots a m) => Cons2 a m (Store2 slots) where
--     cons2 = encodeStore ; {-# INLINE cons2 #-}



    -- instance ( Monad m
    --          , Cons2 (Symbol atom layout) m TermStore
    --          {-constraint solving-}
    --          , layout ~ ModelLayout sys model
    --          , scope  ~ Scope Atom model
    --          , Assert (atom `In` Atoms scope) (InvalidAtomFormat atom scope))
    --       => Cons2 (Symbol atom layout) m (TermData sys model) where
    --     cons2 v = TermData <$> cons2 v ; {-# INLINE cons2 #-}

-- data Stack3 layers a = ShellX3 (List (TermLayer <$> layers <*> '[a])) a
-- makeWrapped ''ShellX3
--
-- class ConsShellX3 ls m where
--     consShellX3 :: m (ShellX3 ls a)
--
--
-- instance Monad m                                    => ConsShellX3 '[]       m where consShellX3 = return $ ShellX3 List.Null
-- instance (Monad m, ConsShellX3 ls m, ConsLayer m l) => ConsShellX3 (l ': ls) m where consShellX3 = (:-:) <$> consLayer <*> (unwrap' <$> consShellX3)




-- foox :: List

-- newtype Layer t a = Layer a deriving (Show, Functor, Traversable, Foldable)

-- data Shell layers a where
--     Layer ::
--
--
-- data Layer t a = Layer (LayerData t a) a



---------------------
-- === Records === --
---------------------


-- === Relations === --

-- | Variants is needed for operations that need to know all possible variant types
--   like maps that should invoke some action on current variant (e.g. `show`).
type family Variants3 rec :: [*]

type family InferVariant rec v :: Constraint

-- === Construction === --

class Monad m => Cons2 v m t where
    cons2 :: v -> m t

instance (Monad m, EncodeStore slots a m) => Cons2 a m (Store2 slots) where
    cons2 = encodeStore ; {-# INLINE cons2 #-}


--
-- instance (Monad m, List.Generate (Cons2 v) m (Unwrapped (Unwrapped (Record dict fields))))
--       => Cons2 v m (Record dict fields) where
--          cons2 v = (Record . TMap) <$> List.generate (Proxy :: Proxy (Cons2 v)) (cons2 v) ; {-# INLINE cons2 #-}


instance (Monad m, List.Generate (Cons2 v) m (Unwrapped (Unwrapped (Record t))))
      => Cons2 v m (Record t) where
         cons2 v = (Record . TMap) <$> List.generate (Proxy :: Proxy (Cons2 v)) (cons2 v) ; {-# INLINE cons2 #-}




type InvalidAtomFormat atom format = 'Text "Atom `" :<>: 'ShowType atom :<>: 'Text "` is not a valid for format `" :<>: 'ShowType format :<>: 'Text "`"

-- instance ( Monad m
--          , Cons2 (Symbol atom dyn bind) m TermStore
--          {-constraint solving-}
--          , dyn  ~ dyn'
--          , bind ~ bind'
--          , Assert (atom `In` Atoms layout) (InvalidAtomFormat atom layout))
--       => Cons2 (Symbol atom dyn bind) m (TermData bind' model (Layout dyn' layout)) where
--     cons2 v = TermData <$> cons2 v ; {-# INLINE cons2 #-}

-- instance ( Monad m
--          , Cons2 (Symbol atom dyn bind) m TermStore
--          {-constraint solving-}
--          , Assert (atom `In` Atoms ll) (InvalidAtomFormat atom ll))
--       => Cons2 (Symbol atom layout) m (TermData bind' model (Layout dyn' ll)) where
--     cons2 v = TermData <$> cons2 v ; {-# INLINE cons2 #-}
--


--
-- instance (Monad m, Cons2 (Symbol atom layout) m (Unwrapped (Term2 sys dict layers model)))
--       => Cons2 (Symbol atom layout) m (Term2 sys dict layers model) where
--          cons2 a = wrap' <$> cons2 a ; {-# INLINE cons2 #-}

instance (Monad m, Cons2 a m (Unwrapped (Term t model)))
      => Cons2 a m (Term t model) where
         cons2 a = wrap' <$> cons2 a ; {-# INLINE cons2 #-}



-- instance ( Monad m
--          , Cons2 (N.NamedSymbol atom name el) m TermStore
--          {-constraint solving-}
--          , name ~ Int
--          , el ~ el'
--          , Assert (atom `In` Atoms ll) (InvalidAtomFormat atom ll))
--       => Cons2 (N.NamedSymbol atom name el) m (TermData el' model (Layout dyn' ll)) where
--     cons2 v = TermData <$> cons2 v ; {-# INLINE cons2 #-}


instance ( Monad m
         , Cons2 (Symbol atom layout) m TermStore
         {-constraint solving-}
         , layout ~ ModelLayout sys model
         , scope  ~ Scope Atom model
         , Assert (atom `In` Atoms scope) (InvalidAtomFormat atom scope))
      => Cons2 (Symbol atom layout) m (TermData sys model) where
    cons2 v = TermData <$> cons2 v ; {-# INLINE cons2 #-}

--
-- instance ( Monad m
--          , Cons2 (Symbol atom layout) m TermStore
--          {-constraint solving-}
--          , layout ~ Layout.Named Int Int -- Bind t sys model
--         --  , Assert (atom `In` Atoms ll) (InvalidAtomFormat atom ll))
--          )
--       => Cons2 (Symbol atom layout) m (TermData t) where
--     cons2 v = TermData <$> cons2 v ; {-# INLINE cons2 #-}


--

instance Monad m => Cons2 v m Int where cons2 _ = return 5



-- -- === Pattern Matching === --
--
-- type family RecordOf2 t
--
-- class HasRecord t where
--     record2 :: Lens' t (RecordOf2 t)
--





----------------------------------------------------



-- Term '[Scope := Draft, Dynamics := Static, Layout := Network] '[Expr, Type, ...]
-- Layout okresla jakie sa polaczenia pomiedzy elementami AST/ASG oraz dla innych warstw jak np. dla Type.
-- UWAGA: dla nodow sa inne polaczenia niz dla Type - dla Type sa edge, dla nodow nie? Czy moze sa takie same?







--







-- !!!!!!!!!!!!!!!!!!!!!! przy konstruktorach robimy tak ze atom wkladamy do monady konstruujacej i odpalamy tworzenie warst. Te ktore beda go chcialy sie do niego dostana. Wtedy bedziemy wiedzieli jak uzyc dokladnie VGRecord


-- newtype     Expr        t fmt dyn sel = Expr (Layout_OLD t fmt dyn sel)
-- type        AnyExpr     t fmt dyn     = Expr         t fmt dyn 'Nothing
-- type        LimitedExpr t fmt dyn a   = Expr         t fmt dyn ('Just a)
-- type        AtomicExpr  t fmt dyn a   = LimitedExpr  t fmt dyn '[a]
--
-- type family Layout_OLD      t fmt dyn (sel :: Maybe [*]) :: *
-- -- type family Layout2     t a :: *
type family TermOf      a
--
-- type family Layout2 t :: * -> *
--
-- -- === Utils === --
--
-- type family Selected (sel :: Maybe [*]) (lst :: [*]) where
--             Selected 'Nothing    lst = lst
--             Selected ('Just sel) lst = sel -- FIXME[WD]: The selection does NOT check if it matches with possible candidates
--
-- type        Variants        t fmt  dyn a bind = Symbols (Selected a (Atoms fmt)) dyn bind
-- type        SubDynExprs     t fmt  dyn        = Expr t fmt <$> SubDynamics     dyn <*> '[ 'Nothing ]
-- type        SubSemiDynExprs t fmt  dyn        = Expr t fmt <$> SubSemiDynamics dyn <*> '[ 'Nothing ]
-- type        SubExprs        t fmt  dyn        = SubExprs' t (SubFormats fmt) dyn
-- type family SubExprs'       t fmts dyn where
--             SubExprs' t '[]           dyn = '[]
--             SubExprs' t '[fmt]        dyn = SubDynExprs     t fmt dyn
--             SubExprs' t (fmt ': fmts) dyn = SubSemiDynExprs t fmt dyn <> SubExprs' t fmts dyn
--
--
-- type        Variants2        fmt  dyn sel a = Symbols (Selected sel (Atoms fmt)) dyn a
--
--
-- -- type        SubDynExprs2     fmt  dyn a      = Term fmt <$> SubDynamics     dyn <*> '[ 'Nothing ] <*> '[ a ]
-- -- type        SubSemiDynExprs2 fmt  dyn a      = Term fmt <$> SubSemiDynamics dyn <*> '[ 'Nothing ] <*> '[ a ]
-- -- type        SubExprs2        fmt  dyn a      = SubExprs2' (SubFormats fmt) dyn a
-- -- type family SubExprs2' fmts          dyn a where
-- --             SubExprs2' '[]           dyn a = '[]
-- --             SubExprs2' '[fmt]        dyn a = SubDynExprs2     fmt dyn a
-- --             SubExprs2' (fmt ': fmts) dyn a = SubSemiDynExprs2 fmt dyn a <> SubExprs2' fmts dyn a


-- -- === Defaults === --
--
-- -- | Standard expr record definition
-- type ExprRecord t fmt dyn sel a = VGRecord (SubExprs t fmt dyn) (Variants t fmt dyn sel a) Data2
-- -- newtype TermData fmt dyn sel a = TermData (VGRecord (SubExprs2 fmt dyn a) (Variants2 fmt dyn sel a) Data2)
--
--
-- -- === Instances === --
--
-- -- Show
-- deriving instance Show (Unwrapped (Expr t fmt dyn sel)) => Show (Expr t fmt dyn sel)
--
-- -- Relations
-- type instance TermOf   (Expr t fmt dyn sel) = Expr t fmt dyn sel
-- type instance Base     (Expr t fmt dyn sel) = fmt
-- type instance RecordOf (Expr t fmt dyn sel) = RecordOf (Unwrapped (Expr t fmt dyn sel))
--
-- -- Wrapper
-- makeWrapped ''Expr
--
-- -- Record
-- instance IsRecord  (Unwrapped (Expr t fmt dyn a)) => IsRecord  (Expr t fmt dyn a) where asRecord = wrapped' ∘ asRecord ; {-^. INLINE asRecord ^.-}
-- instance HasRecord (Unwrapped (Expr t fmt dyn a)) => HasRecord (Expr t fmt dyn a) where record   = wrapped' ∘ record   ; {-^. INLINE record   #-}
--
-- -- Shell
-- instance Shell.HasLayer' l (Unwrapped (Expr t fmt dyn sel)) => Shell.HasLayer' l (Expr t fmt dyn sel) where
--     layer' = wrapped' ∘ layer' ; {-# INLINE layer' #-}
--
--
--
-- -------------------------
-- -- === OverBuilder === --
-- -------------------------
--
-- class Monad m => OverBuilder m a where
--     overbuild :: RecordOf a -> m a
--
-- instance Monad m => OverBuilder m (VGRecord gs vs d) where
--     overbuild = return ; {-# INLINE overbuild #-}
--
-- instance (Monad m, OverBuilder m (Unwrapped (Expr t fmt dyn a))) => OverBuilder m (Expr t fmt dyn a) where
--     overbuild = Expr <∘> overbuild ; {-# INLINE overbuild #-}



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
