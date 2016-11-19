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
{-# LANGUAGE UndecidableSuperClasses #-}

{-# LANGUAGE GADTs #-}





module Luna.IR.Model.Internal where


import           Prelude                      (curry)
import           Luna.Prelude                 hiding (Register, register, elem, head, tail, curry, Field2, Enum, Num, Swapped, Curry, String, Integer, Rational, Symbol, Index, Data, Field, Updater', update')
import qualified Luna.Prelude                 as P

import           Data.Abstract ()
import           Data.Base
import           Data.Record                  hiding (Layout, Variants, SymbolMap, symbolMap, Match, Cons, Value, cons, Group, HasValue, ValueOf, value)
import qualified Data.Record                  as Record
import           Type.Cache.TH                (assertTypesEq, cacheHelper, cacheType)
import           Type.Container               hiding (Empty, FromJust, Every)
import           Type.Map

import           Data.Typeable                (splitTyConApp, tyConName, typeRepTyCon)
import           Old.Luna.Runtime.Dynamics      (Dynamics, Dynamic, Static, SubDynamics, SubSemiDynamics, ByDynamics)
import qualified Old.Luna.Runtime.Dynamics      as Dynamics
import           Luna.IR.Repr.Styles
import           Luna.IR.Function.Argument
import           Data.Reprx
import           Type.Bool
import           Luna.IR.Expr.Format
import Luna.IR.Expr.Symbol (Sym, Symbol, IsSymbol, symbol, UncheckedFromSymbol, FromSymbol, uncheckedFromSymbol, fromSymbol, ToSymbol, toSymbol, UniSymbol, uniSymbol, IsUniSymbol)
import qualified Luna.IR.Expr.Symbol.Named as N
import Luna.IR.Expr.Atom

-- import Data.Shell               as Shell hiding (Access)
import Data.Record.Model.Masked as X (TermRecord, VGRecord2, Store2(Store2), Slot(Slot), Enum(Enum))
import Type.Monoid
import Type.Applicative

import Prologue.Unsafe (error)
-- import Luna.IR.Expr (NameByDynamics)
import qualified Luna.IR.Expr.Symbol as Symbol
import qualified Data.RTuple as List
import Type.Promotion    (KnownNats, natVals)
import Data.Bits         (setBit, zeroBits)

import Data.RTuple (List, Empty, empty)
import Data.Record.Model.Masked (encode2, EncodeStore, encodeStore, Mask, encodeNat, encodeData2, checkData2, decodeData2, Raw(Raw), unsafeRestore, decodeNat)
import           Data.RTuple (TMap(..), empty, Assoc(..), Assocs, (:=:)) -- refactor empty to another library

import GHC.TypeLits (ErrorMessage(Text, ShowType, (:<>:)))
import Type.Error
-- import Control.Monad.State
import Data.Property
import qualified Data.Property as Prop
import GHC.Stack (HasCallStack, callStack, prettyCallStack, withFrozenCallStack)
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as V
import Control.Monad.ST (ST, runST)
import Type.List (Index, Size)
import Type.Maybe (FromJust)
import Data.Phantom
import Unsafe.Coerce     (unsafeCoerce)
import Type.Relation (SemiSuper)
import qualified Luna.IR.Expr.Layout as Layout
import Luna.IR.Expr.Layout (Layout, LayoutOf, Name, Generalize, Universal, universal)
import Type.Inference

import qualified Data.Set as Data (Set)
import qualified Data.Set as Set

import Data.Container.List (ToSet, toSet)
import GHC.Prim (Any)

import           Control.Monad.Event     hiding (Any)
import qualified Control.Monad.Event     as Event

import Type.Container (Every)


import Luna.IR.Model.Layer
import Luna.IR.Model.Layer.Data


import Data.Coerced (unsafeCoerced)


import qualified Data.Hetero.Stack as Stack
import           Data.Hetero.Stack (Stack)




--------------------
-- === Events === --
--------------------

-- === Definition === --

type Register        t a m = Event        (t (Universal a)) m (Universal a)
type DelayedRegister t a m = DelayedEvent (t (Universal a)) m (Universal a)


-- === Registration === --

register :: forall t a m. Register t a m => a -> m a
register a = a <$ dispatch_ @(t (Universal a)) (universal a) ; {-# INLINE register #-}

delayedRegister :: forall t a m. DelayedRegister t a m => a -> m a
delayedRegister a = a <$ delayedDispatch_ @(t (Universal a)) (universal a) ; {-# INLINE delayedRegister #-}


-- === Event types === --

data New a










------------------
-- === Elem === --
------------------

newtype Elem a = Elem Any
makeWrapped '' Elem

type instance Definition t (Elem a) = Definition t a


-- === Classes === --

class IsElem a where
    elem :: Iso' a (Elem a)
    default elem :: (Wrapped a, Unwrapped a ~ Elem a) => Iso' a (Elem a)
    elem = wrapped' ; {-# INLINE elem #-}


-- === Instances === --

instance KnownRepr a => Show (Elem a) where
    show _ = typeRepr @a ; {-# INLINE show #-}



-----------------
-- === IR === --
-----------------

newtype IRT  t m a = IRT (IdentityT m a) deriving (Functor, Traversable, Foldable, Applicative, Monad, MonadTrans, MonadIO, MonadFix)
newtype IR   t   a = IR  a               deriving (Functor, Traversable, Foldable)
type    IRMT   m   = IRT (Cfg m) m
type    IRM    m   = IR  (Cfg m)

data IRType = IRType deriving (Show)


-- === IR building === --

type family Definition t a

definition :: IsElem a => Iso' (IR t a) (Definition t a)
definition = unsafeIRWrapped . elem . wrapped' ∘ unsafeCoerced ; {-# INLINE definition #-}

fromDefinition :: (IRMonad m, IsElem a) => Definition (Cfg m) a -> m a
fromDefinition = liftIR . view (from definition) ; {-# INLINE fromDefinition #-}

toDefinition :: (IRMonad m, IsElem a) => a -> m (Definition (Cfg m) a)
toDefinition = view definition <∘> mark' ; {-# INLINE toDefinition #-}


-- === IRMonad === ---

class                         Monad m           => IRMonad m          where liftIR :: forall a. IR (Cfg m) a -> m a
instance {-# OVERLAPPABLE #-} Monad m           => IRMonad (IRT t m) where liftIR = return . unsafeIRUnwrap ; {-# INLINE liftIR #-}
instance {-# OVERLAPPABLE #-} IRMonadTrans t m => IRMonad (t      m) where liftIR = lift . liftIR           ; {-# INLINE liftIR #-}

type IRMonadTrans t m = (IRMonad m, MonadTrans t, Monad (t m), Cfg m ~ Cfg (t m))

-- FIXME[WD]: maybe we should relax a bit the Cfg definition?
type family Cfg (m :: * -> *) where
    Cfg (IRT t m) = t
    Cfg (IR  t)   = t
    Cfg (m t)      = Cfg t


-- === IR mark === ---

type Marked m a = IRM m (IRVal a)

class                          Monad m                    => Markable m a            where mark :: a -> m (Marked m a)
instance {-# OVERLAPPABLE #-} (Monad m, IRVal a ~ a)     => Markable m a            where mark = return . IR ; {-# INLINE mark #-}
instance {-# OVERLAPPABLE #-} (Monad m, t ~ Cfg m)        => Markable m (IR  t   a) where mark = return       ; {-# INLINE mark #-}
instance {-# OVERLAPPABLE #-} (Monad m, t ~ Cfg m, m ~ n) => Markable m (IRT t n a) where mark = runIRT      ; {-# INLINE mark #-}

type family IRVal a where
    IRVal (IR  _   a) = a
    IRVal (IRT _ _ a) = a
    IRVal a            = a

mark' :: IRMonad m => a -> m (IRM m a)
mark' = return . IR ; {-# INLINE mark' #-}


-- === Running === --

transform :: Monad m => IR t a -> IRT t m a
transform = return . unsafeIRUnwrap ; {-# INLINE transform #-}

runIRT :: Functor m => IRT t m a -> m (IR t a)
runIRT (IRT m) = IR . runIdentityT m ; {-# INLINE runIRT #-}

unsafeIRUnwrap :: IR t a -> a
unsafeIRUnwrap (IR a) = a ; {-# INLINE unsafeIRUnwrap #-}

unsafeIRWrapped :: Iso' (IR t a) a
unsafeIRWrapped = iso unsafeIRUnwrap IR ; {-# INLINE unsafeIRWrapped #-}


-- === Property selectors === --

type Select   p m a =  Access    p (Marked m a)
type Selector p m a = (Accessor  p (Marked m a), Markable m a)
type Updaterx p m a = (Updater'  p (Marked m a), Markable m a, IRMonad m)

select :: forall p m a. Selector p m a => a -> m (Select p m a)
select = access @p <∘> mark ; {-# INLINE select #-}

updatex :: forall p m a. Updaterx p m a => Select p m a -> a -> m (IRVal a)
updatex v a = (liftIR . update' @p v) =<< mark a ; {-# INLINE updatex #-}

with :: forall p m a. (Selector p m a, Updaterx p m a) => (Select p m a -> Select p m a) -> a -> m (IRVal a)
with f src = do
    succs <- select @p src
    updatex @p (f succs) src
{-# INLINE with #-}


-- === Instances === --

-- Show
instance {-# OVERLAPPABLE #-}                              Show (IR I a) where show = impossible
instance (Show (Definition t a), IsElem a, KnownRepr a) => Show (IR t a) where
    showsPrec d a = showParen' d $ showString (typeRepr @a <> " ") . showsPrec' (a ^. definition)

-- PrimMonad
instance PrimMonad m => PrimMonad (IRT t m) where
    type PrimState (IRT t m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}

-- Properties
type instance Access p (IR t a) = Access p (Definition t a)

instance (Accessor p (Definition t a), IsElem a) => Accessor p (IR t a) where
    access = access @p . view definition ; {-# INLINE access #-}

instance (Updater' p (Definition t a), IsElem a) => Updater' p (IR t a) where
    update' v = definition %~ update' @p v ; {-# INLINE update' #-}

-- Universal
type instance Universal (IR t a) = IR t (Universal a)

-- Ordering
instance (Ord (Definition t a), IsElem a) => Ord (IR t a) where compare = compare `on` (^. definition) ; {-# INLINE compare #-}
instance (Eq  (Definition t a), IsElem a) => Eq  (IR t a) where (==)    = (==)    `on` (^. definition) ; {-# INLINE (==)    #-}


------------------------
-- === Data Layer === --
------------------------


instance {-# OVERLAPPABLE #-} (Monad m, a ~ LayerData Data t) => Constructor a m (Layer t Data) where
    cons = return . Layer ; {-# INLINE cons #-}




------------------------
-- === LayerStack === --
------------------------

type    LayerStackBase a   = Stack (Layer a)
newtype LayerStack     t a = LayerStack (LayerStackBase (IR t a) (Layers (Universal a) t))
makeWrapped ''LayerStack


-- === Lenses === --

-- class IsLayerStack a where
--     layerStack2 :: Iso' a (LayerStack a)
--     default layerStack2 :: (Wrapped a, Unwrapped a ~ LayerStack a) => Iso' a (LayerStack a)
--     layerStack2 = wrapped' ; {-# INLINE layerStack2 #-}
--
--
-- === StackCons === --

-- type LayerStackCons m a = StackCons (Layers (Struct a) (Cfg m)) m
--
-- consLayerStack :: LayerStackCons m a => LayerData Data a -> m (LayerStack (Cfg m) a)
consLayerStack a = LayerStack <$> consStack a

type StackStepCons l ls m = (StackCons ls m, LayerCons l m)
class    Monad m              => StackCons ls        m where consStack :: forall t. LayerData Data t -> m (LayerStackBase t ls)
instance Monad m              => StackCons '[]       m where consStack _ = return def                                 ; {-# INLINE consStack #-}
instance StackStepCons l ls m => StackCons (l ': ls) m where consStack d = Stack.push <$> consLayer d <*> consStack d ; {-# INLINE consStack #-}


-- === HasLayer === --

class HasLayer   t q layer where layer' :: forall a. Lens' (LayerStackBase a (Layers q t)) (LayerData layer a)
type  HasLayers  t q layers = Constraints (HasLayer t q <$> layers)
type  HasLayerM  m q layer  = HasLayer  (Cfg m) q layer
type  HasLayersM m q layers = HasLayers (Cfg m) q layers

instance {-# OVERLAPPABLE #-} Stack.HasLayer layer (Layers q t)
      => HasLayer t q layer where layer' = Stack.layer @layer @(Layers q t) . wrapped' ; {-# INLINE layer' #-}
instance HasLayer I q layer where layer' = impossible                                  ; {-# INLINE layer' #-}


-- -- === Instances === --

deriving instance Show (Unwrapped (LayerStack t a)) => Show (LayerStack t a)

type instance Access p (LayerStack t a) = LayerData p (IR t a)

instance HasLayer t (Universal a) p => Accessor p (LayerStack t a) where
    access = view (layer' @t @(Universal a) @p) . unwrap' ; {-# INLINE access #-}

instance HasLayer t (Universal a) p => Updater' p (LayerStack t a) where
    update' v = (wrapped' . layer' @t @(Universal a) @p) .~ v ; {-# INLINE update' #-}

-- FIXME[WD]: after refactoring out the Constructors this could be removed vvv
instance (Monad m, Constructor v m (Unwrapped (LayerStack t a))) => Constructor v m (LayerStack t a) where cons a = wrap' <$> cons a





------------------------
-- === References === --
------------------------

-- === Definition === --

newtype Ref a = Ref (Elem (Ref a))

type instance Definition t (Ref a) = Impl Ref (Universal a) t a
instance      IsElem       (Ref a)

type family Impl (f :: * -> *) i t :: * -> *

makeWrapped ''Ref


--- === Operations === --

-- Refs

type Referable' m a = Referable (Universal a) (Cfg m) m
class Monad m => Referable i t m where
    refDesc    :: forall a. (i ~ Universal a, t ~ Cfg m) => IR t a                     -> m (Ref a)
    unrefDesc  :: forall a. (i ~ Universal a, t ~ Cfg m) => IR t (Ref a)               -> m ()
    readDesc   :: forall a. (i ~ Universal a, t ~ Cfg m) => IR t (Ref a)               -> m a
    writeDesc  :: forall a. (i ~ Universal a, t ~ Cfg m) => IR t (Ref a) -> a          -> m ()
    modifyDesc :: forall a. (i ~ Universal a, t ~ Cfg m) => IR t (Ref a) -> (a -> m a) -> m ()
    modifyDesc ref f = writeDesc ref =<< f =<< readDesc ref ; {-# INLINE modifyDesc #-}

type IRValLike t m a = (Markable m t, IRVal t ~ a)

silentRef :: (Referable' m a, IRValLike t m a) => t -> m (Ref a)
silentRef = refDesc <=< mark ; {-# INLINE silentRef #-}

ref :: (Referable' m a, IRValLike t m a, Register New (Ref a) m) => t -> m (Ref a)
ref = register @New <=< silentRef ; {-# INLINE ref #-}

delayedRef :: (Referable' m a, IRValLike t m a, DelayedRegister New (Ref a) m) => t -> m (Ref a)
delayedRef = delayedRegister @New <=< silentRef ; {-# INLINE delayedRef #-}

read :: (Referable' m a, IRValLike t m (Ref a)) => t -> m a
read = readDesc <=< mark ; {-# INLINE read #-}

write :: (Referable' m a, IRValLike t m (Ref a)) => t -> a -> m ()
write d a = flip writeDesc a =<< mark d ; {-# INLINE write #-}

modify :: (Referable' m a, IRValLike t m (Ref a)) => t -> (a -> m a) -> m ()
modify d f = flip modifyDesc f =<< mark d ; {-# INLINE modify #-}

modify' :: (Referable' m a, IRValLike t m (Ref a)) => t -> (a -> a) -> m ()
modify' d = modify d . fmap return ; {-# INLINE modify' #-}


-- TODO: tak moze wygladac taktyka na pure functions:
-- IR t (Ref a) -> (g ->) (Ast t a)
--       (Ref a) -> m             a
--


-- class Monad m => ExprStore m where
--     exprs  :: m [Ref Expr']
--     -- links  :: m [Ref ExprLink']



-- === Instances === --

-- Basic
deriving instance Eq   (Unwrapped (Ref a)) => Eq   (Ref a)
deriving instance Ord  (Unwrapped (Ref a)) => Ord  (Ref a)

-- Repr
type instance TypeRepr (Ref _) = "Ref"
instance      Show     (Ref a) where show = show . unwrap' ; {-# INLINE show #-}

-- Struct
type instance Universal (Ref a) = Ref (Universal a)


-- Generalize
instance {-# OVERLAPPABLE #-} (Generalize a b, t ~ Ref b) => Generalize (Ref a) t
instance {-# OVERLAPPABLE #-} (Generalize a b, t ~ Ref a) => Generalize t       (Ref b)
instance {-# OVERLAPPABLE #-} (Generalize a b)            => Generalize (Ref a) (Ref b)



------------------
-- === Link === --
------------------

newtype Link   src tgt = Link (Elem (Link src tgt))
type    Link'  a       = Link a a
type    SubLink c t    = Ref (Link (Sub c t) t)
type family SubLink2 (a :: *) (b :: *) where SubLink2 c (IR t a) = IR t (Ref (Link (Sub c a) a))


type instance Definition t    (Link src tgt) = LayerStack t (Link src tgt)
instance      IsElem          (Link src tgt)
type instance LayerData  Data (IR t (Link src tgt)) = (IR t (Ref src), IR t (Ref tgt))

makeWrapped ''Link


-- -- === Construction === --

type LayerStackCons m a = StackCons (Layers (Universal a) (Cfg m)) m -- REFACTORME
type Linkable' src tgt m = (IRMonad m, LayerStackCons m (Link src tgt))

link' :: Linkable' src tgt m => Ref src -> Ref tgt -> m (Link src tgt)
link' a b = fromDefinition =<< consLayerStack =<< ((,) <$> mark' a <*> mark' b) ; {-# INLINE link' #-}

link :: (Linkable' src tgt m, Referable' m (Link src tgt), Register New (Ref (Link src tgt)) m) => Ref src -> Ref tgt -> m (Ref (Link src tgt))
link = ref <=<< link' ; {-# INLINE link #-}

delayedLink :: forall src tgt n m. (Linkable' src tgt m, Referable' m (Link src tgt), DelayedRegister New (Ref (Link src tgt)) m)
             => Ref src -> Ref tgt -> m (Ref (Link src tgt))
delayedLink = delayedRef <=<< link' ; {-# INLINE delayedLink #-}


-- === Instances === --

-- Struct
type instance Universal (Link src tgt) = Link (Universal src) (Universal tgt)

-- Repr
type instance TypeRepr (Link _ _) = "Link"
instance      Show     (Link src tgt) where show = show . unwrap' ; {-# INLINE show #-}

-- -- LayerStack
-- instance IsLayerStack (Link src tgt)
--
-- -- Properties
-- type instance Access p (Link src tgt) = Access p (Unwrapped (Link src tgt))
-- instance HasLayer' (Link src tgt) p => Accessor  p (Link src tgt) where access    = view $ layer @p ; {-# INLINE access  #-}
-- instance HasLayer' (Link src tgt) p => Updater' p (Link src tgt) where update' a = layer @p .~ a   ; {-# INLINE update' #-}
--





------------------------
-- === ExprSymbol === --
------------------------

newtype ExprSymbol    atom t = ExprSymbol (N.Symbol atom (Layout.Named (SubLink Name t) (SubLink Atom t)))
type    ExprSymbol'   atom   = ExprSymbol atom Layout.Any
newtype ExprUniSymbol      t = ExprUniSymbol (N.UniSymbol (Layout.Named (SubLink Name t) (SubLink Atom t)))
makeWrapped ''ExprSymbol
makeWrapped ''ExprUniSymbol


-- === Helpers === --

hideLayout :: ExprSymbol atom t -> ExprSymbol atom Layout.Any
hideLayout = unsafeCoerce ; {-# INLINE hideLayout #-}


-- === Layout validation === ---
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
type ValidateLayout_ model sel a = ValidateScope (model # sel) sel a
type ValidateLayout' t     sel a = ValidateLayout (t # Layout) sel a


-- === Instances === --

-- FIXME: [WD]: it seems that Layout in the below declaration is something else than real layout - check it and refactor
type instance Access Layout (ExprSymbol atom t) = Access Layout (Unwrapped (ExprSymbol atom t))
type instance Access Atom   (ExprSymbol atom t) = atom
type instance Access Format (ExprSymbol atom t) = Access Format atom
type instance Access Sym    (ExprSymbol atom t) = ExprSymbol atom t

instance Accessor Sym (ExprSymbol atom t) where access = id ; {-# INLINE access #-}

instance UncheckedFromSymbol (ExprSymbol atom t) where uncheckedFromSymbol = wrap' ; {-# INLINE uncheckedFromSymbol #-}

instance ValidateLayout (LayoutOf t) Atom atom
      => FromSymbol (ExprSymbol atom t) where fromSymbol = wrap' ; {-# INLINE fromSymbol #-}


-- Repr
instance Repr s (Unwrapped (ExprSymbol atom t))
      => Repr s (ExprSymbol atom t) where repr = repr . unwrap' ; {-# INLINE repr #-}

-- Fields
type instance FieldsType (ExprSymbol atom t) = FieldsType (Unwrapped (ExprSymbol atom t))
instance HasFields (Unwrapped (ExprSymbol atom t))
      => HasFields (ExprSymbol atom t) where fieldList = fieldList . unwrap' ; {-# INLINE fieldList #-}



----------------------
-- === TermData === --
----------------------

type TermStoreSlots = '[ Atom ':= Enum, Format ':= Mask, Sym ':= Raw ]
type TermStore = Store2 TermStoreSlots

newtype TermData sys model = TermData TermStore deriving (Show)
makeWrapped ''TermData


-- === Encoding === --

class                                                              SymbolEncoder atom where encodeSymbol :: forall t. ExprSymbol atom t -> TermStore
instance                                                           SymbolEncoder I    where encodeSymbol = impossible
instance EncodeStore TermStoreSlots (ExprSymbol' atom) Identity => SymbolEncoder atom where
    encodeSymbol = runIdentity . encodeStore . hideLayout ; {-# INLINE encodeSymbol #-} -- magic


------------------
-- === Expr === --
------------------

-- === Definition === --

newtype Expr layout = Expr (Elem (Expr layout))
type    Expr'       = Expr Draft
type    AnyExpr     = Expr Layout.Any

type instance Definition t    (Expr layout) = LayerStack t (Expr layout)
instance      IsElem          (Expr layout)
type instance LayerData  Data (IR t (Expr layout)) = TermStore

makeWrapped ''Expr
-- makeRepr    ''Expr


-- === Instances === --

-- Struct
type instance Universal (Expr _)      = Expr'
type instance Sub     s (Expr layout) = Expr (Sub s layout)

-- Repr
type instance TypeRepr (Expr _) = "Expr"
instance Show (Expr layout) where show = show . unwrap' ; {-# INLINE show #-}

-- Properties
type instance LayoutOf (Expr layout) = layout


-- === Utils === --

uniExprTypes2 :: (expr ~ Expr layout, sym ~ ExprSymbol atom expr) => IR t expr -> sym -> sym
uniExprTypes2 _ = id ; {-# INLINE uniExprTypes2 #-}

unsafeSpecifyLayout2 :: AnyExpr -> Expr layout
unsafeSpecifyLayout2 = unsafeCoerce ; {-# INLINE unsafeSpecifyLayout2 #-}

anyLayout3 :: Ref (Expr layout) -> Ref (Expr Layout.Any)
anyLayout3 = unsafeCoerce



-- === Construction === --

type ExprBuilder      m = (IRMonad m, Constructor TermStore m (Definition (Cfg m) (Elem AnyExpr)))
type SilentExprCons   m = (ExprBuilder m, Referable Expr' (Cfg m) m)
type ExprMonad        m = (SilentExprCons m, Register        New (Ref Expr') m)
type DelayedExprMonad m = (SilentExprCons m, DelayedRegister New (Ref Expr') m)

buildExpr :: (ExprBuilder m, SymbolEncoder atom) => ExprSymbol atom (Expr layout) -> m (Expr layout)
buildExpr a = fmap unsafeSpecifyLayout2 . fromDefinition =<< cons (encodeSymbol a) ; {-# INLINE buildExpr #-}

silentExpr :: (SilentExprCons m, SymbolEncoder atom) => ExprSymbol atom (Expr layout) -> m (Ref (Expr layout))
silentExpr = silentRef <=< buildExpr ; {-# INLINE silentExpr #-}

expr :: (ExprMonad m, SymbolEncoder atom) => ExprSymbol atom (Expr layout) -> m (Ref (Expr layout))
expr = ref <=< buildExpr ; {-# INLINE expr #-}

delayedExpr :: (DelayedExprMonad m, SymbolEncoder atom) => ExprSymbol atom (Expr layout) -> m (Ref (Expr layout))
delayedExpr = delayedRef <=< buildExpr ; {-# INLINE delayedExpr #-}


-- === Symbol mapping === --

class Monad m => SymbolMapM' (atoms :: [*]) ctx expr m b where
    symbolMapM' :: (forall a. ctx a m b => a -> m b) -> expr -> m b

type SymbolMapM_AMB = SymbolMapM' (Every Atom)
symbolMapM_AMB :: forall ctx m expr b. SymbolMapM_AMB ctx expr m b => (forall a. ctx a m b => a -> m b) -> expr -> m b
symbolMapM_AMB = symbolMapM' @(Every Atom) @ctx ; {-# INLINE symbolMapM_AMB #-}

type SymbolMapM_AB ctx      = SymbolMapM_AMB (DropMonad ctx)
type SymbolMap_AB  ctx expr = SymbolMapM_AB ctx expr Identity
symbolMapM_AB :: forall ctx expr m b. SymbolMapM_AB ctx expr m b => (forall a. ctx a b => a -> b) -> expr -> m b
symbolMapM_AB f = symbolMapM_AMB @(DropMonad ctx) (return <$> f) ; {-# INLINE symbolMapM_AB #-}

symbolMap_AB :: forall ctx expr b. SymbolMap_AB ctx expr b => (forall a. ctx a b => a -> b) -> expr -> b
symbolMap_AB f = runIdentity . symbolMapM_AB @ctx f ; {-# INLINE symbolMap_AB #-}

type SymbolMapM_A ctx = SymbolMapM_AB (FreeResult ctx)
type SymbolMap_A  ctx expr = SymbolMapM_A ctx expr Identity
symbolMapM_A :: forall ctx expr m b. SymbolMapM_A ctx expr m b => (forall a. ctx a => a -> b) -> expr -> m b
symbolMapM_A = symbolMapM_AB @(FreeResult ctx) ; {-# INLINE symbolMapM_A #-}

symbolMap_A :: forall ctx expr b. SymbolMap_A ctx expr b => (forall a. ctx a => a -> b) -> expr -> b
symbolMap_A f = runIdentity . symbolMapM_A @ctx f ; {-# INLINE symbolMap_A #-}

class    (ctx a b, Monad m) => DropMonad ctx a m b
instance (ctx a b, Monad m) => DropMonad ctx a m b

class    ctx a => FreeResult ctx a b
instance ctx a => FreeResult ctx a b

instance ( ctx (ExprSymbol a (Expr layout)) m b
         , SymbolMapM' as ctx (Expr layout) m b
         , idx ~ FromJust (Encode2 Atom a) -- FIXME: make it nicer
         , KnownNat idx
         , HasLayerM m Expr' Data
         )
      => SymbolMapM' (a ': as) ctx (Expr layout) m b where
    symbolMapM' f expr = do
        d <- unwrap' <$> select @Data expr
        let eidx = unwrap' $ access @Atom d
            idx  = fromIntegral $ natVal (Proxy :: Proxy idx)
            sym  = unsafeCoerce (unwrap' $ access @Sym d) :: ExprSymbol a (Expr layout)
        if (idx == eidx) then f sym else symbolMapM' @as @ctx f expr

instance ( ctx (ExprSymbol a (Expr layout)) m b
         , SymbolMapM' as ctx (IR t (Expr layout)) m b
         , idx ~ FromJust (Encode2 Atom a) -- FIXME: make it nicer
         , KnownNat idx
         , HasLayer t Expr' Data
         )
      => SymbolMapM' (a ': as) ctx (IR t (Expr layout)) m b where
    symbolMapM' f expr = do
        let d    = unwrap' $ access @Data expr
            eidx = unwrap' $ access @Atom d
            idx  = fromIntegral $ natVal (Proxy :: Proxy idx)
            sym  = unsafeCoerce (unwrap' $ access @Sym d) :: ExprSymbol a (Expr layout)
        if (idx == eidx) then f sym else symbolMapM' @as @ctx f expr

instance Monad m => SymbolMapM' '[] ctx expr m b where symbolMapM' _ _ = impossible


instance HasLayer t Expr' Data => Repr HeaderOnly (IR t (Expr layout)) where repr expr = symbolMap_A @(Repr HeaderOnly) repr expr


class HasFields2 a b where fieldList2 :: a -> b
instance (b ~ [FieldsType a], HasFields a) => HasFields2 a b where fieldList2 = fieldList

-- WARNING: works only for Drafts for now as it assumes that the child-refs have the same type as the parent
-- type FieldsC t layout = SymbolMap2 HasFields2 (Expr t layout) [Ref (Link (Expr t layout) (Expr t layout))]
symbolFields :: (SymbolMap_AB HasFields2 (IR t expr) out, expr ~ Expr layout, out ~ [Ref (Link expr expr)]) => IR t expr -> out
symbolFields = symbolMap_AB @HasFields2 fieldList2



-- class     IsUniSymbol t l where
--     uniSymbol :: Symbol t l -> UniSymbol l

class IsUniSymbol2 a b where uniSymbol2 :: a -> b
instance (Unwrapped a ~ Symbol t l, b ~ UniSymbol l, IsUniSymbol t l, Wrapped a)
      => IsUniSymbol2 a b where uniSymbol2 = uniSymbol . unwrap' ; {-# INLINE uniSymbol2 #-}

-- exprUniSymbol :: SymbolMap_AB IsUniSymbol2 expr b => expr -> b
-- exprUniSymbol = symbolMap_AB @IsUniSymbol2 uniSymbol2

exprUniSymbol :: HasLayer t Expr' Data => (IR t (Expr layout)) -> ExprUniSymbol (Expr layout)
exprUniSymbol = ExprUniSymbol . symbolMap_AB @IsUniSymbol2 uniSymbol2


-- symbolMap_AB :: forall ctx expr b. SymbolMap_AB ctx expr b => (forall a. ctx a b => a -> b) -> expr -> b

-------------------------------------
-- === Expr Layout type caches === --
-------------------------------------

type instance Encode2 Atom    v = Index v (Every Atom)
type instance Encode2 Format  v = Index v (Every Format)







--
-- newtype Just' a  = Just' { fromJust' :: a } deriving (Show, Functor, Foldable, Traversable)
-- data    Nothing' = Nothing' deriving (Show)
--




--------------------------------------------

-- type family ValueOf a
--
-- class HasValue a where
--     value :: a -> ValueOf a
--
-- type instance ValueOf (Just' a, _) = a
-- type instance ValueOf (Nothing',_) = ()
--
-- instance HasValue (Just' a , t) where value   = fromJust' . fst
-- instance HasValue (Nothing', t) where value _ = ()
--
-- type ResultDesc  m a = m (Just' a , OutputDesc m)
-- type ResultDesc_ m   = m (Nothing', OutputDesc m)
--
--
--
-- type NoOutput m = (OutputDesc m ~ Nothing')
--
--
--
-- class IsResult m where
--     toResult  :: forall a. ResultDesc  m a -> Result  m a
--     toResult_ ::           ResultDesc_ m   -> Result_ m
--
-- instance {-# OVERLAPPABLE #-} IsResult ((->) t) where
--     toResult desc = do
--         (Just' a, Just' b) <- desc
--         return (a, b)
--
--     toResult_ desc = fromJust' . snd <$> desc
--
-- instance {-# OVERLAPPABLE #-} (OutputDesc m ~ Nothing', Monad m) => IsResult m where
--     toResult  desc = fromJust' . fst <$> desc ; {-# INLINE toResult  #-}
--     toResult_ desc = return ()                ; {-# INLINE toResult_ #-}
--
-- --------------------------------------------
--
--
-- -- type Result m a = m (Output m a)
-- --
-- -- type family Output m a where
-- --     Output ((->) t) () = t
-- --     Output ((->) t) a  = (t,a)
-- --     Output m        a  = a
--
-- type Result  m a = m (Output (OutputDesc m) a)
-- type Result_ m   = m (Output_ (OutputDesc m))
--
-- type family Output arg a where
--     Output (Just' t) a = (a,t)
--     Output Nothing'  a = a
--
-- type family Output_ arg where
--     Output_ (Just' t) = t
--     Output_ Nothing'  = ()
--
--
--
-- type family OutputDesc m where
--     OutputDesc ((->) t) = Just' t
--     OutputDesc m        = Nothing'
--
-- -- type NoResult m = Output m () ~ ()
