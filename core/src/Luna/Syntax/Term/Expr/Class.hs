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





module Luna.Syntax.Term.Expr.Class where


import           Prelude                      (curry)
import           Prelude.Luna                 hiding (Register, register, elem, head, tail, curry, Field2, Enum, Num, Swapped, Curry, String, Integer, Rational, Symbol, Index, Data, Field, Setter', set')
import qualified Prelude.Luna                 as P

import           Data.Abstract ()
import           Data.Base
import           Data.Record                  hiding (Layout, Variants, SymbolMap, symbolMap, Match, Cons, Value, cons, Group, HasValue, ValueOf, value)
import qualified Data.Record                  as Record
import           Type.Cache.TH                (assertTypesEq, cacheHelper, cacheType)
import           Type.Container               hiding (Empty, FromJust, Every)
import           Type.Map

import           Data.Typeable                (splitTyConApp, tyConName, typeRepTyCon)
import           Luna.Runtime.Dynamics      (Dynamics, Dynamic, Static, SubDynamics, SubSemiDynamics, ByDynamics)
import qualified Luna.Runtime.Dynamics      as Dynamics
import           Luna.Pretty.Styles
import           Luna.Syntax.Term.Function.Argument
import           Data.Reprx
import           Type.Bool
import           Luna.Syntax.Term.Expr.Format
import Luna.Syntax.Term.Expr.Symbol (Sym, Symbol, IsSymbol, symbol, UncheckedFromSymbol, FromSymbol, uncheckedFromSymbol, fromSymbol, ToSymbol, toSymbol, UniSymbol, uniSymbol, IsUniSymbol)
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
import qualified Luna.Syntax.Term.Expr.Layout as Layout
import Luna.Syntax.Term.Expr.Layout (Layout, LayoutOf, Name, Generalize, Universal, universal)
import Type.Inference

import qualified Data.Set as Data (Set)
import qualified Data.Set as Set

import Data.Container.List (ToSet, toSet)
import GHC.Prim (Any)

import           Control.Monad.Event2     hiding (Any)
import qualified Control.Monad.Event2     as Event

import Type.Container (Every)




import Data.Coerced (unsafeCoerced)






-----------------------------------------------------------------------------------------
-- DEPRECIATED DEPRECIATED DEPRECIATED DEPRECIATED DEPRECIATED DEPRECIATED DEPRECIATED --
-----------------------------------------------------------------------------------------

class Monad m => Constructor a m t where
    cons :: a -> m t

-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------








data a := b

type instance Get t (l  := v ': ls) = If (t == l) v (Get t ls)
type instance Get t (l ':= v ': ls) = If (t == l) v (Get t ls)









-- === Properties === --

-- TODO: refactor
data System   = System   deriving (Show)
data TermType = TermType deriving (Show)






--------------------
-- === Events === --
--------------------


data New a


type Register        t a m = Event        (t (Universal a)) m (Universal a)
type DelayedRegister t a m = DelayedEvent (t (Universal a)) m (Universal a)

register :: forall t a m. Register t a m => a -> m a
register a = a <$ dispatch_ @(t (Universal a)) (universal a) ; {-# INLINE register #-}

delayedRegister :: forall t a m. DelayedRegister t a m => a -> m a
delayedRegister a = a <$ delayedDispatch_ @(t (Universal a)) (universal a) ; {-# INLINE delayedRegister #-}


-------------------
-- === Stack === --
-------------------

data Stack (t :: ★ -> ★) layers where
    SLayer :: t l -> Stack t ls -> Stack t (l ': ls)
    SNull  :: Stack t '[]


-- === Utils === --

head :: Lens' (Stack t (l ': ls)) (t l)
head = lens (\(SLayer a _) -> a) (\(SLayer _ s) a -> SLayer a s) ; {-# INLINE head #-}

tail :: Lens' (Stack t (l ': ls)) (Stack t ls)
tail = lens (\(SLayer _ s) -> s) (\(SLayer a _) s -> SLayer a s) ; {-# INLINE tail #-}


-- === StackHasLayers === --

class                                          StackHasLayer l ls        where stackLayer :: forall t. Lens' (Stack t ls) (t l)
instance {-# OVERLAPPABLE #-}                  StackHasLayer l (l ': ls) where stackLayer = head          ; {-# INLINE stackLayer #-}
instance {-# OVERLAPPABLE #-} StackHasLayer l ls => StackHasLayer l (t ': ls) where stackLayer = tail . stackLayer ; {-# INLINE stackLayer #-}


-- === Instances === --

-- Show
instance ContentShow (Stack t ls)               => Show          (Stack t ls       )  where show s                        = "(" <> contentShow s <> ")"      ; {-# INLINE show #-}
instance                                           Show (Content (Stack t '[]      )) where show _                        = ""                               ; {-# INLINE show #-}
instance (Show (t l), ContentShow (Stack t ls)) => Show (Content (Stack t (l ': ls))) where show (unwrap' -> SLayer l ls) = show l <> ", " <> contentShow ls ; {-# INLINE show #-}
instance {-# OVERLAPPING #-} Show (t l)         => Show (Content (Stack t '[l]     )) where show (unwrap' -> SLayer l ls) = show l                           ; {-# INLINE show #-}

-- Constructor
instance ( Constructor a m (t l)
         , Constructor a m (Stack t ls)) => Constructor a m (Stack t (l ': ls)) where cons a = SLayer <$> cons a <*> cons a ; {-# INLINE cons #-}
instance Monad m                         => Constructor a m (Stack t '[]      ) where cons _ = return SNull                 ; {-# INLINE cons #-}


-- Properties
type instance Get p (Stack t ls) = t p

instance {-# OVERLAPPABLE #-}                           Getter  p (Stack t (p ': ls)) where get    (SLayer t _) = t                   ; {-# INLINE get #-}
instance {-# OVERLAPPABLE #-} Getter p (Stack t ls)  => Getter  p (Stack t (l ': ls)) where get    (SLayer _ l) = get @p l            ; {-# INLINE get #-}

instance {-# OVERLAPPABLE #-}                           Setter' p (Stack t (p ': ls)) where set' a (SLayer _ s) = SLayer a s          ; {-# INLINE set' #-}
instance {-# OVERLAPPABLE #-} Setter' p (Stack t ls) => Setter' p (Stack t (l ': ls)) where set' a (SLayer t s) = SLayer t (set' a s) ; {-# INLINE set' #-}






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
-- === Asg === --
-----------------

newtype AsgT  t m a = AsgT (IdentityT m a) deriving (Functor, Traversable, Foldable, Applicative, Monad, MonadTrans, MonadIO, MonadFix)
newtype Asg   t   a = Asg  a               deriving (Functor, Traversable, Foldable)
type    AsgMT   m   = AsgT (Cfg m) m
type    AsgM    m   = Asg  (Cfg m)


-- === Asg building === --

type family Definition t a

definition :: IsElem a => Iso' (Asg t a) (Definition t a)
definition = unsafeAsgWrapped . elem . wrapped' ∘ unsafeCoerced ; {-# INLINE definition #-}

fromDefinition :: (AsgMonad m, IsElem a) => Definition (Cfg m) a -> m a
fromDefinition = liftAsg . view (from definition) ; {-# INLINE fromDefinition #-}

toDefinition :: (AsgMonad m, IsElem a) => a -> m (Definition (Cfg m) a)
toDefinition = view definition <∘> mark' ; {-# INLINE toDefinition #-}


-- === AsgMonad === ---

class                         Monad m           => AsgMonad m          where liftAsg :: forall a. Asg (Cfg m) a -> m a
instance {-# OVERLAPPABLE #-} Monad m           => AsgMonad (AsgT t m) where liftAsg = return . unsafeAsgUnwrap ; {-# INLINE liftAsg #-}
instance {-# OVERLAPPABLE #-} AsgMonadTrans t m => AsgMonad (t      m) where liftAsg = lift . liftAsg           ; {-# INLINE liftAsg #-}

type AsgMonadTrans t m = (AsgMonad m, MonadTrans t, Monad (t m), Cfg m ~ Cfg (t m))

-- FIXME[WD]: maybe we should relax a bit the Cfg definition?
type family Cfg (m :: * -> *) where
    Cfg (AsgT t m) = t
    Cfg (Asg  t)   = t
    Cfg (m t)      = Cfg t


-- === Asg mark === ---

type Marked m a = AsgM m (AsgVal a)

class                          Monad m                    => Markable m a            where mark :: a -> m (Marked m a)
instance {-# OVERLAPPABLE #-} (Monad m, AsgVal a ~ a)     => Markable m a            where mark = return . Asg ; {-# INLINE mark #-}
instance {-# OVERLAPPABLE #-} (Monad m, t ~ Cfg m)        => Markable m (Asg  t   a) where mark = return       ; {-# INLINE mark #-}
instance {-# OVERLAPPABLE #-} (Monad m, t ~ Cfg m, m ~ n) => Markable m (AsgT t n a) where mark = runAsgT      ; {-# INLINE mark #-}

type family AsgVal a where
    AsgVal (Asg  _   a) = a
    AsgVal (AsgT _ _ a) = a
    AsgVal a            = a

mark' :: AsgMonad m => a -> m (AsgM m a)
mark' = return . Asg ; {-# INLINE mark' #-}


-- === Running === --

transform :: Monad m => Asg t a -> AsgT t m a
transform = return . unsafeAsgUnwrap ; {-# INLINE transform #-}

runAsgT :: Functor m => AsgT t m a -> m (Asg t a)
runAsgT (AsgT m) = Asg . runIdentityT m ; {-# INLINE runAsgT #-}

unsafeAsgUnwrap :: Asg t a -> a
unsafeAsgUnwrap (Asg a) = a ; {-# INLINE unsafeAsgUnwrap #-}

unsafeAsgWrapped :: Iso' (Asg t a) a
unsafeAsgWrapped = iso unsafeAsgUnwrap Asg ; {-# INLINE unsafeAsgWrapped #-}


-- === Property selectors === --

type Select   p m a =  Get     p (Marked m a)
type Selector p m a = (Getter  p (Marked m a), Markable m a)
type Updater  p m a = (Setter' p (Marked m a), Markable m a, AsgMonad m)

select :: forall p m a. Selector p m a => a -> m (Select p m a)
select = get @p <∘> mark ; {-# INLINE select #-}

update :: forall p m a. Updater p m a => Select p m a -> a -> m (AsgVal a)
update v a = (liftAsg . set' @p v) =<< mark a ; {-# INLINE update #-}

with :: forall p m a. (Selector p m a, Updater p m a) => (Select p m a -> Select p m a) -> a -> m (AsgVal a)
with f src = do
    succs <- select @p src
    update @p (f succs) src
{-# INLINE with #-}


-- === Instances === --

-- Show
instance {-# OVERLAPPABLE #-}                              Show (Asg I a) where show = impossible
instance (Show (Definition t a), IsElem a, KnownRepr a) => Show (Asg t a) where
    showsPrec d a = showParen' d $ showString (typeRepr @a <> " ") . showsPrec' (a ^. definition)

-- PrimMonad
instance PrimMonad m => PrimMonad (AsgT t m) where
    type PrimState (AsgT t m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}

-- Properties
type instance Get p (Asg t a) = Get p (Definition t a)

instance (Getter p (Definition t a), IsElem a) => Getter p (Asg t a) where
    get = get @p . view definition ; {-# INLINE get #-}

instance (Setter' p (Definition t a), IsElem a) => Setter' p (Asg t a) where
    set' v = definition %~ set' @p v ; {-# INLINE set' #-}

-- Universal
type instance Universal (Asg t a) = Asg t (Universal a)

-- Ordering
instance (Ord (Definition t a), IsElem a) => Ord (Asg t a) where compare = compare `on` (^. definition) ; {-# INLINE compare #-}
instance (Eq  (Definition t a), IsElem a) => Eq  (Asg t a) where (==)    = (==)    `on` (^. definition) ; {-# INLINE (==)    #-}


------------------------
-- === Data Layer === --
------------------------

data Data = Data deriving (Show)

instance {-# OVERLAPPABLE #-} (Monad m, a ~ LayerData Data t) => Constructor a m (Layer t Data) where
    cons = return . Layer ; {-# INLINE cons #-}



--------------------
-- === Layers === --
--------------------

-- === Definition === --

type family LayerData l t

newtype     Layer  t l = Layer (LayerData l t)
type family Layers q a :: [*]

makeWrapped ''Layer


-- === Classes === --

class Monad m => LayerCons l m where
    consLayer :: forall t. LayerData Data t -> m (Layer t l)


-- === Isntances === --

deriving instance Show (Unwrapped (Layer t l))
      => Show (Layer t l)

instance Default (Unwrapped (Layer t l))
      => Default (Layer t l) where def = wrap' def ; {-# INLINE def #-}


------------------------
-- === LayerStack === --
------------------------

type    LayerStackBase a   = Stack (Layer a)
newtype LayerStack     t a = LayerStack (LayerStackBase (Asg t a) (Layers (Universal a) t))
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
instance Monad m              => StackCons '[]       m where consStack _ = return SNull                           ; {-# INLINE consStack #-}
instance StackStepCons l ls m => StackCons (l ': ls) m where consStack d = SLayer <$> consLayer d <*> consStack d ; {-# INLINE consStack #-}


-- === HasLayer === --

class HasLayer   t q layer where layer' :: forall a. Lens' (LayerStackBase a (Layers q t)) (LayerData layer a)
type  HasLayers  t q layers = Constraints (HasLayer t q <$> layers)
type  HasLayerM  m q layer  = HasLayer  (Cfg m) q layer
type  HasLayersM m q layers = HasLayers (Cfg m) q layers

instance {-# OVERLAPPABLE #-} StackHasLayer layer (Layers q t)
      => HasLayer t q layer where layer' = stackLayer @layer @(Layers q t) . wrapped' ; {-# INLINE layer' #-}
instance HasLayer I q layer where layer' = impossible                                 ; {-# INLINE layer' #-}


-- -- === Instances === --

deriving instance Show (Unwrapped (LayerStack t a)) => Show (LayerStack t a)

type instance Get p (LayerStack t a) = LayerData p (Asg t a)

instance HasLayer t (Universal a) p => Getter p (LayerStack t a) where
    get = view (layer' @t @(Universal a) @p) . unwrap' ; {-# INLINE get #-}

instance HasLayer t (Universal a) p => Setter' p (LayerStack t a) where
    set' v = (wrapped' . layer' @t @(Universal a) @p) .~ v ; {-# INLINE set' #-}

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
    refDesc    :: forall a. (i ~ Universal a, t ~ Cfg m) => Asg t a                     -> m (Ref a)
    unrefDesc  :: forall a. (i ~ Universal a, t ~ Cfg m) => Asg t (Ref a)               -> m ()
    readDesc   :: forall a. (i ~ Universal a, t ~ Cfg m) => Asg t (Ref a)               -> m a
    writeDesc  :: forall a. (i ~ Universal a, t ~ Cfg m) => Asg t (Ref a) -> a          -> m ()
    modifyDesc :: forall a. (i ~ Universal a, t ~ Cfg m) => Asg t (Ref a) -> (a -> m a) -> m ()
    modifyDesc ref f = writeDesc ref =<< f =<< readDesc ref ; {-# INLINE modifyDesc #-}

type AsgValLike t m a = (Markable m t, AsgVal t ~ a)

silentRef :: (Referable' m a, AsgValLike t m a) => t -> m (Ref a)
silentRef = refDesc <=< mark ; {-# INLINE silentRef #-}

ref :: (Referable' m a, AsgValLike t m a, Register New (Ref a) m) => t -> m (Ref a)
ref = register @New <=< silentRef ; {-# INLINE ref #-}

delayedRef :: (Referable' m a, AsgValLike t m a, DelayedRegister New (Ref a) m) => t -> m (Ref a)
delayedRef = delayedRegister @New <=< silentRef ; {-# INLINE delayedRef #-}

read :: (Referable' m a, AsgValLike t m (Ref a)) => t -> m a
read = readDesc <=< mark ; {-# INLINE read #-}

write :: (Referable' m a, AsgValLike t m (Ref a)) => t -> a -> m ()
write d a = flip writeDesc a =<< mark d ; {-# INLINE write #-}

modify :: (Referable' m a, AsgValLike t m (Ref a)) => t -> (a -> m a) -> m ()
modify d f = flip modifyDesc f =<< mark d ; {-# INLINE modify #-}

modify' :: (Referable' m a, AsgValLike t m (Ref a)) => t -> (a -> a) -> m ()
modify' d = modify d . fmap return ; {-# INLINE modify' #-}


-- TODO: tak moze wygladac taktyka na pure functions:
-- Asg t (Ref a) -> (g ->) (Ast t a)
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
type family SubLink2 (a :: *) (b :: *) where SubLink2 c (Asg t a) = Asg t (Ref (Link (Sub c a) a))


type instance Definition t    (Link src tgt) = LayerStack t (Link src tgt)
instance      IsElem          (Link src tgt)
type instance LayerData  Data (Asg t (Link src tgt)) = (Asg t (Ref src), Asg t (Ref tgt))

makeWrapped ''Link


-- -- === Construction === --

type LayerStackCons m a = StackCons (Layers (Universal a) (Cfg m)) m -- REFACTORME
type Linkable' src tgt m = (AsgMonad m, LayerStackCons m (Link src tgt))

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
-- type instance Get p (Link src tgt) = Get p (Unwrapped (Link src tgt))
-- instance HasLayer' (Link src tgt) p => Getter  p (Link src tgt) where get    = view $ layer @p ; {-# INLINE get  #-}
-- instance HasLayer' (Link src tgt) p => Setter' p (Link src tgt) where set' a = layer @p .~ a   ; {-# INLINE set' #-}
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
type ValidateLayout_ model sel a = ValidateScope (model ^. sel) sel a
type ValidateLayout' t     sel a = ValidateLayout (t ^. Layout) sel a


-- === Instances === --

-- FIXME: [WD]: it seems that Layout in the below declaration is something else than real layout - check it and refactor
type instance Get Layout (ExprSymbol atom t) = Get Layout (Unwrapped (ExprSymbol atom t))
type instance Get Atom   (ExprSymbol atom t) = atom
type instance Get Format (ExprSymbol atom t) = Get Format atom
type instance Get Sym    (ExprSymbol atom t) = ExprSymbol atom t

instance Getter Sym (ExprSymbol atom t) where get = id ; {-# INLINE get #-}

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
type instance LayerData  Data (Asg t (Expr layout)) = TermStore

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

uniExprTypes2 :: (expr ~ Expr layout, sym ~ ExprSymbol atom expr) => Asg t expr -> sym -> sym
uniExprTypes2 _ = id ; {-# INLINE uniExprTypes2 #-}

unsafeSpecifyLayout2 :: AnyExpr -> Expr layout
unsafeSpecifyLayout2 = unsafeCoerce ; {-# INLINE unsafeSpecifyLayout2 #-}

anyLayout3 :: Ref (Expr layout) -> Ref (Expr Layout.Any)
anyLayout3 = unsafeCoerce



-- === Construction === --

type ExprBuilder      m = (AsgMonad m, Constructor TermStore m (Definition (Cfg m) (Elem AnyExpr)))
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
        let eidx = unwrap' $ get @Atom d
            idx  = fromIntegral $ natVal (Proxy :: Proxy idx)
            sym  = unsafeCoerce (unwrap' $ get @Sym d) :: ExprSymbol a (Expr layout)
        if (idx == eidx) then f sym else symbolMapM' @as @ctx f expr

instance ( ctx (ExprSymbol a (Expr layout)) m b
         , SymbolMapM' as ctx (Asg t (Expr layout)) m b
         , idx ~ FromJust (Encode2 Atom a) -- FIXME: make it nicer
         , KnownNat idx
         , HasLayer t Expr' Data
         )
      => SymbolMapM' (a ': as) ctx (Asg t (Expr layout)) m b where
    symbolMapM' f expr = do
        let d    = unwrap' $ get @Data expr
            eidx = unwrap' $ get @Atom d
            idx  = fromIntegral $ natVal (Proxy :: Proxy idx)
            sym  = unsafeCoerce (unwrap' $ get @Sym d) :: ExprSymbol a (Expr layout)
        if (idx == eidx) then f sym else symbolMapM' @as @ctx f expr

instance Monad m => SymbolMapM' '[] ctx expr m b where symbolMapM' _ _ = impossible


instance HasLayer t Expr' Data => Repr HeaderOnly (Asg t (Expr layout)) where repr expr = symbolMap_A @(Repr HeaderOnly) repr expr


class HasFields2 a b where fieldList2 :: a -> b
instance (b ~ [FieldsType a], HasFields a) => HasFields2 a b where fieldList2 = fieldList

-- WARNING: works only for Drafts for now as it assumes that the child-refs have the same type as the parent
-- type FieldsC t layout = SymbolMap2 HasFields2 (Expr t layout) [Ref (Link (Expr t layout) (Expr t layout))]
symbolFields :: (SymbolMap_AB HasFields2 (Asg t expr) out, expr ~ Expr layout, out ~ [Ref (Link expr expr)]) => Asg t expr -> out
symbolFields = symbolMap_AB @HasFields2 fieldList2



-- class     IsUniSymbol t l where
--     uniSymbol :: Symbol t l -> UniSymbol l

class IsUniSymbol2 a b where uniSymbol2 :: a -> b
instance (Unwrapped a ~ Symbol t l, b ~ UniSymbol l, IsUniSymbol t l, Wrapped a)
      => IsUniSymbol2 a b where uniSymbol2 = uniSymbol . unwrap' ; {-# INLINE uniSymbol2 #-}

-- exprUniSymbol :: SymbolMap_AB IsUniSymbol2 expr b => expr -> b
-- exprUniSymbol = symbolMap_AB @IsUniSymbol2 uniSymbol2

exprUniSymbol :: HasLayer t Expr' Data => (Asg t (Expr layout)) -> ExprUniSymbol (Expr layout)
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
