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


import           Prelude                      (curry)
import           Prelude.Luna                 hiding (head, tail, curry, Field2, Enum, Num, Swapped, Curry, String, Integer, Rational, Symbol, Index, Data, Field, Setter', set')
import qualified Prelude.Luna                 as P

import           Data.Abstract
import           Data.Base
import           Data.Record                  hiding (Layout, Variants, SymbolMap, symbolMap, Match, Cons, Value, cons, Group, HasValue, ValueOf, value)
import qualified Data.Record                  as Record
import           Type.Cache.TH                (assertTypesEq, cacheHelper, cacheType)
import           Type.Container               hiding (Empty, FromJust)
import           Type.Map

import           Data.Typeable                (splitTyConApp, tyConName, typeRepTyCon)
import           Luna.Runtime.Dynamics      (Dynamics, Dynamic, Static, SubDynamics, SubSemiDynamics, ByDynamics)
import qualified Luna.Runtime.Dynamics      as Dynamics
import           Luna.Pretty.Styles
import           Luna.Syntax.Term.Function.Argument
import           Data.Reprx
import           Type.Bool
import           Luna.Syntax.Term.Expr.Format
import Luna.Syntax.Term.Expr.Symbol (Sym, Symbol, IsSymbol, symbol, FromSymbol, fromSymbol, ToSymbol, toSymbol)
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
import qualified Luna.Syntax.Term.Expr.Layout as Layout
import Luna.Syntax.Term.Expr.Layout (Layout, Name, Generalize, Universal)
import Type.Inference

import qualified Data.Set as Data (Set)
import qualified Data.Set as Set

import Data.Container.List (ToSet, toSet)
import Data.Ident


-- import Data.Graph.Model.Edge (Edge) -- Should be removed as too deep dependency?
-- data {-kind-} Layout dyn form = Layout dyn form deriving (Show)
--
-- type instance Get Dynamics (Layout dyn form) = dyn
-- type instance Get Format   (Layout dyn form) = form





type family All a :: [*]
type instance All Atom = '[Acc, App, Blank, Cons, Lam, Match, Missing, Native, Star, Unify, Var]


newtype Just' a  = Just' { fromJust' :: a } deriving (Show, Functor, Foldable, Traversable)
data    Nothing' = Nothing' deriving (Show)





--------------------------------------------

type family ValueOf a

class HasValue a where
    value :: a -> ValueOf a

type instance ValueOf (Just' a, _) = a
type instance ValueOf (Nothing',_) = ()

instance HasValue (Just' a , t) where value   = fromJust' . fst
instance HasValue (Nothing', t) where value _ = ()

type ResultDesc  m a = m (Just' a , OutputDesc m)
type ResultDesc_ m   = m (Nothing', OutputDesc m)



type NoOutput m = (OutputDesc m ~ Nothing')



class IsResult m where
    toResult  :: forall a. ResultDesc  m a -> Result  m a
    toResult_ ::           ResultDesc_ m   -> Result_ m

instance {-# OVERLAPPABLE #-} IsResult ((->) t) where
    toResult desc = do
        (Just' a, Just' b) <- desc
        return (a, b)

    toResult_ desc = fromJust' . snd <$> desc

instance {-# OVERLAPPABLE #-} (OutputDesc m ~ Nothing', Monad m) => IsResult m where
    toResult  desc = fromJust' . fst <$> desc ; {-# INLINE toResult  #-}
    toResult_ desc = return ()                ; {-# INLINE toResult_ #-}

--------------------------------------------


-- type Result m a = m (Output m a)
--
-- type family Output m a where
--     Output ((->) t) () = t
--     Output ((->) t) a  = (t,a)
--     Output m        a  = a

type Result  m a = m (Output (OutputDesc m) a)
type Result_ m   = m (Output_ (OutputDesc m))

type family Output arg a where
    Output (Just' t) a = (a,t)
    Output Nothing'  a = a

type family Output_ arg where
    Output_ (Just' t) = t
    Output_ Nothing'  = ()



type family OutputDesc m where
    OutputDesc ((->) t) = Just' t
    OutputDesc m        = Nothing'

-- type NoResult m = Output m () ~ ()




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






-- TODO: zamiast zahardcodoewanego `a` mozna uzywac polimorficznego, ktorego poszczegolne komponenty jak @Data bylyby wymuszane przez poszczegolne warstwy
-- expr2 :: (Constructor a m (ExprStack t layers layout), expr ~ Expr t layers layout, a ~ ExprSymbol atom expr) => a -> m expr
-- expr2 a = Expr <$> cons a

-- | The `expr` type does not force the construction to be checked,
--   because it has to be already performed in order to deliver ExprSymbol.








data a := b

type instance Get t (l  := v ': ls) = If (t == l) v (Get t ls)
type instance Get t (l ':= v ': ls) = If (t == l) v (Get t ls)



type PossibleVariants = [Acc, App, Blank, Cons, Lam, Match, Missing, Native, Star, Unify, Var]
type PossibleFormats  = [Literal, Value, Thunk, Phrase, Draft]





-- === Properties === --

-- TODO: refactor
data Data     = Data     deriving (Show)
data System   = System   deriving (Show)
data TermType = TermType deriving (Show)








-------------------------
-- === Connections === --
-------------------------


-- === Definition === --

type family Cfg2 a

type family Cfg2MergeImpl c c'
type family Cfg2Merge c c' where
    Cfg2Merge c c  = c
    Cfg2Merge c c' = Cfg2MergeImpl c c'



type family Impl (f :: * -> *) i t :: * -> *
type Impl' f a = Impl f (Ident a) (Cfg2 a) a

newtype Ref   a   = Ref   (Impl' Ref   a)



--- === Operations === --

-- Refs

type Referable' a m = Referable (Ident a) (Cfg2 a) m
class (Monad m, IsResult m) => Referable i t m where
    refDesc     :: forall a. (i ~ Ident a, t ~ Cfg2 a) =>     a               -> ResultDesc  m (Ref a)
    unrefDesc   :: forall a. (i ~ Ident a, t ~ Cfg2 a) => Ref a               -> ResultDesc_ m
    readDesc    :: forall a. (i ~ Ident a, t ~ Cfg2 a) => Ref a               ->             m a
    writeDesc   :: forall a. (i ~ Ident a, t ~ Cfg2 a) => Ref a -> a          -> ResultDesc_ m
    modifyMDesc :: forall a. (i ~ Ident a, t ~ Cfg2 a) => Ref a -> (a -> m a) -> ResultDesc_ m
    modifyMDesc ref f = writeDesc ref =<< f =<< readDesc ref ; {-# INLINE modifyMDesc #-}

silentRef  :: Referable' a m => a -> Result  m (Ref a)
silentRef' :: Referable' a m => a ->         m (Ref a)
silentRef  = toResult ∘  refDesc ; {-# INLINE silentRef  #-}
silentRef' = value   <∘> refDesc ; {-# INLINE silentRef' #-}

unref  :: Referable' a m => Ref a -> Result_ m
unref' :: Referable' a m => Ref a ->         m ()
unref  = toResult_ ∘  unrefDesc ; {-# INLINE unref  #-}
unref' = value    <∘> unrefDesc ; {-# INLINE unref' #-}

read  :: Referable' a m => Ref a -> m a
read' :: Referable' a m => Ref a -> m a
read  = readDesc ; {-# INLINE read  #-}
read' = readDesc ; {-# INLINE read' #-}

write  :: Referable' a m => Ref a -> a -> Result_ m
write' :: Referable' a m => Ref a -> a ->         m ()
write  = toResult_ ∘∘  writeDesc ; {-# INLINE write  #-}
write' = value    <∘∘> writeDesc ; {-# INLINE write' #-}

modifyM  :: Referable' a m => Ref a -> (a -> m a) -> Result_ m
modifyM' :: Referable' a m => Ref a -> (a -> m a) ->         m ()
modifyM  = toResult_ ∘∘  modifyMDesc ; {-# INLINE modifyM  #-}
modifyM' = value    <∘∘> modifyMDesc ; {-# INLINE modifyM' #-}

modify  :: Referable' a m => Ref a -> (a -> a) -> Result_ m
modify' :: Referable' a m => Ref a -> (a -> a) ->         m ()
modify  ref = modifyM  ref ∘ fmap return ; {-# INLINE modify  #-}
modify' ref = modifyM' ref ∘ fmap return ; {-# INLINE modify' #-}


-- === Instances === --

-- Wrappers
makeWrapped ''Ref

-- Ident
type instance Ident (Ref a) = Ref (Ident a)

-- Universal
type instance Universal (Ref a) = Ref (Universal a)

-- Basic
deriving instance Show (Unwrapped (Ref a)) => Show (Ref a)
deriving instance Eq   (Unwrapped (Ref a)) => Eq   (Ref a)
deriving instance Ord  (Unwrapped (Ref a)) => Ord  (Ref a)

-- Generalize
instance {-# OVERLAPPABLE #-} (Generalize a b, t ~ Ref b) => Generalize (Ref a) t
instance {-# OVERLAPPABLE #-} (Generalize a b, t ~ Ref a) => Generalize t       (Ref b)
instance {-# OVERLAPPABLE #-} (Generalize a b)            => Generalize (Ref a) (Ref b)











-- type Result m a = m (Output (OutputDesc m) a)
--
-- type family Output arg a where
--     Output ('Just t) a = (a,t)
--     Output 'Nothing  a = a
--
-- type family OutputDesc m where
--     OutputDesc ((->) t) = 'Just t
--     OutputDesc m        = 'Nothing
--
-- ref'3 ::




-- Groups

-- type Groupable' a m = Groupable (Cfg a) m
-- class Monad m => Groupable t m where
--     groupM'   :: forall a. [Ref a] -> m (Impl Group t a)
--     ungroupM' :: forall a. Impl Group t a -> m [Ref a]
--
-- groupM   :: forall a b m. Groupable' a m => [Ref a] -> m (Group a)
-- ungroupM :: forall a b m. Groupable' a m => Group a -> m [Ref a]
-- groupM   = Group <∘> groupM' @(Cfg a)   ; {-# INLINE groupM #-}
-- ungroupM = ungroupM' @(Cfg a) . unwrap' ; {-# INLINE ungroupM #-}











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


-- === Selectors === --

class                                          Selector l ls        where select :: forall t. Lens' (Stack t ls) (t l)
instance {-# OVERLAPPABLE #-}                  Selector l (l ': ls) where select = head          ; {-# INLINE select #-}
instance {-# OVERLAPPABLE #-} Selector l ls => Selector l (t ': ls) where select = tail . select ; {-# INLINE select #-}


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





--------------------
-- === Layers === --
--------------------

-- === Definition === --

type family LayerData l t

newtype Layer t l = Layer (LayerData l t)
makeWrapped ''Layer


-- === Families === --

type family Layers q a :: [*]
type        Layers'  a = Layers (Ident a) (Cfg2 a)


-- === Classes === --

class Monad m => LayerCons l m where
    consLayer :: forall t. LayerData Data t -> m (Layer t l)


-- === Isntances === --

deriving instance Show (Unwrapped (Layer t l)) => Show (Layer t l)

instance Default (Unwrapped (Layer t l)) => Default (Layer t l) where def = wrap' def ; {-# INLINE def #-}


------------------------
-- === LayerStack === --
------------------------

-- === Definition === --

type    LayerStackBase t = Stack (Layer t)
newtype LayerStack     t = LayerStack (LayerStackBase t (Layers' t))
makeWrapped ''LayerStack


-- === Lenses === --

class IsLayerStack a where
    layerStack :: Iso' a (LayerStack a)
    default layerStack :: (Wrapped a, Unwrapped a ~ LayerStack a) => Iso' a (LayerStack a)
    layerStack = wrapped' ; {-# INLINE layerStack #-}


-- === StackCons === --

type StackStepCons l ls m = (StackCons ls m, LayerCons l m)
class    Monad m              => StackCons ls        m where consStack :: forall t. LayerData Data t -> m (LayerStackBase t ls)
instance Monad m              => StackCons '[]       m where consStack _ = return SNull                           ; {-# INLINE consStack #-}
instance StackStepCons l ls m => StackCons (l ': ls) m where consStack d = SLayer <$> consLayer d <*> consStack d ; {-# INLINE consStack #-}


-- === HasLayer === --

type HasLayer' a layer = HasLayer (Ident a) (Cfg2 a) layer

layer :: forall layer a. (HasLayer' a layer, IsLayerStack a) => Lens' a (LayerData layer a)
layer = layerStack . wrapped' . layer' @(Ident a) @(Cfg2 a) @layer ; {-# INLINE layer #-}

class                                                        HasLayer q c layer where layer' :: forall t. Lens' (LayerStackBase t (Layers q c)) (LayerData layer t)
instance {-# OVERLAPPABLE #-} Selector layer (Layers q c) => HasLayer q c layer where layer' = select @layer @(Layers q c) . wrapped' ; {-# INLINE layer' #-}
instance {-# OVERLAPPABLE #-}                                HasLayer q I layer where layer' = impossible                             ; {-# INLINE layer' #-}

type family HasLayers q c ls :: Constraint where
            HasLayers q c '[]       = ()
            HasLayers q c (l ': ls) = (HasLayer q c l, HasLayers q c ls)


-- === Instances === --

deriving instance Show (Unwrapped (LayerStack t)) => Show (LayerStack t)

type instance Get p (LayerStack t) = LayerData p t

-- FIXME[WD]: after refactoring out the Constructors this could be removed vvv
instance (Monad m, Constructor a m (Unwrapped (LayerStack t))) => Constructor a m (LayerStack t) where cons a = wrap' <$> cons a



------------------
-- === Link === --
------------------

type LinkStack src tgt = LayerStack (Link src tgt)

type    Link' a       = Link a a
newtype Link  src tgt = Link (LinkStack src tgt)
makeWrapped ''Link

data LINK
type instance Ident (Link src tgt) = LINK

type instance LayerData Data (Link src tgt) = (Ref src, Ref tgt)

type SubLink c t = Ref (Link (Sub c t) t)


-- === Construction === --

type Linkable  t       m = StackCons (Layers LINK t) m
type Linkable' src tgt m = Linkable (Cfg2 (Link src tgt)) m

link' :: Linkable' src tgt m => Ref src -> Ref tgt -> m (Link src tgt)
link' a b = Link . LayerStack <$> consStack (a,b)


-- === Instances === --

-- Show
deriving instance Show (Unwrapped (Link  a b)) => Show (Link  a b)

-- Cfg
type instance Cfg2 (Link a b) = Cfg2Merge (Cfg2 a) (Cfg2 b)

-- LayerStack
instance IsLayerStack (Link src tgt)

-- Properties
type instance Get p (Link src tgt) = Get p (Unwrapped (Link src tgt))
instance HasLayer' (Link src tgt) p => Getter  p (Link src tgt) where get    = view $ layer @p ; {-# INLINE get  #-}
instance HasLayer' (Link src tgt) p => Setter' p (Link src tgt) where set' a = layer @p .~ a   ; {-# INLINE set' #-}

-- Universal
type instance Universal (Link src tgt) = Link (Universal src) (Universal tgt)



-------------------
-- === Group === --
-------------------

type GroupStack a = LayerStack (Group a)

newtype Group  a = Group (GroupStack a)
makeWrapped ''Group

data GROUP
type instance Ident (Group a) = GROUP

type instance LayerData Data (Group a) = Data.Set (Ref a)


-- === Construction === --

type Groupable  t m = StackCons (Layers GROUP t) m
type Groupable' a m = Groupable (Cfg2 (Group a)) m

group' :: (Groupable' a m, ToSet t, Item t ~ Ref a) => t -> m (Group a)
group' a = Group . LayerStack <$> consStack (toSet a) ; {-# INLINE group' #-}


-- === Instances === --

-- Show
deriving instance Show (Unwrapped (Group a)) => Show (Group a)

-- Cfg
type instance Cfg2 (Group a) = Cfg2 a

-- LayerStack
instance IsLayerStack (Group a)

-- Properties
type instance Get p (Group a) = Get p (Unwrapped (Group a))
instance HasLayer' (Group a) p => Getter  p (Group a) where get    = view $ layer @p ; {-# INLINE get  #-}
instance HasLayer' (Group a) p => Setter' p (Group a) where set' a = layer @p .~ a   ; {-# INLINE set' #-}

-- Universal
type instance Universal (Group a) = Group (Universal a)



------------------------
-- === ExprSymbol === --
------------------------

newtype ExprSymbol  atom t = ExprSymbol (N.NamedSymbol atom (SubLink Name t) (SubLink Atom t))
type    ExprSymbol' atom   = ExprSymbol atom Layout.Any
makeWrapped ''ExprSymbol


-- === Helpers === --

hideLayout :: ExprSymbol atom t -> ExprSymbol atom Layout.Any
hideLayout = unsafeCoerce ; {-# INLINE hideLayout #-}


-- === Instances === --

-- FIXME: [WD]: it seems that Layout in the below declaration is something else than real layout - check it and refactor
type instance Get Layout (ExprSymbol atom t) = Get Layout (Unwrapped (ExprSymbol atom t))
type instance Get Atom   (ExprSymbol atom t) = atom
type instance Get Format (ExprSymbol atom t) = Get Format atom
type instance Get Sym    (ExprSymbol atom t) = ExprSymbol atom t

instance Getter Sym (ExprSymbol atom t) where get = id ; {-# INLINE get #-}

instance ValidateLayout (Get Layout t) Atom atom
      => FromSymbol (ExprSymbol atom t) where fromSymbol = wrap' ; {-# INLINE fromSymbol #-}


-- Repr
instance Repr s (Unwrapped (ExprSymbol atom t))
      => Repr s (ExprSymbol atom t) where repr = repr . unwrap' ; {-# INLINE repr #-}

-- Fields
type instance FieldsType (ExprSymbol atom t) = FieldsType (Unwrapped (ExprSymbol atom t))
instance HasFields (Unwrapped (ExprSymbol atom t)) => HasFields (ExprSymbol atom t) where fieldList = fieldList . unwrap' ; {-# INLINE fieldList #-}



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

-- === Definitions === --

type ExprStack    t layout = LayerStack (Expr t layout)
type AnyExprStack t        = ExprStack t Layout.Any

newtype Expr    t layout = Expr (ExprStack t layout)
type    AnyExpr t        = Expr t Layout.Any
makeWrapped ''Expr

data EXPR
type instance Ident (Expr _ _) = EXPR


-- === Utils === --

mkExpr :: (SymbolEncoder atom, Constructor TermStore m (AnyExprStack t), expr ~ Expr t layout, Referable' expr m) => ExprSymbol atom expr -> m (Ref expr)
mkExpr = silentRef' <=< expr


expr :: (SymbolEncoder atom, Constructor TermStore m (AnyExprStack t), expr ~ Expr t layout) => ExprSymbol atom expr -> m expr
expr a = specifyLayout . Expr <$> cons (encodeSymbol a)

uniExprTypes :: (expr ~ Expr t layout, sym ~ ExprSymbol atom expr) => expr -> sym -> sym
uniExprTypes _ = id ; {-# INLINE uniExprTypes #-}

-- TODO: refactor vvv
specifyLayout :: AnyExpr t -> Expr t layout
specifyLayout = unsafeCoerce ; {-# INLINE specifyLayout #-}



-- === Symbol mapping === --

type  SymbolMap = SymbolMap' (All Atom)
class SymbolMap' (atoms :: [*]) ctx expr where
    symbolMap' :: (forall a. ctx a => a -> b) -> expr -> b

symbolMap :: forall ctx expr b. SymbolMap ctx expr => (forall a. ctx a => a -> b) -> expr -> b
symbolMap = symbolMap' @(All Atom) @ctx ; {-# INLINE symbolMap #-}

instance ( ctx (ExprSymbol a (Expr t layout))
         , SymbolMap' as ctx (Expr t layout)
         , idx ~ FromJust (Encode2 Atom a) -- FIXME: make it nicer
         , KnownNat idx, HasLayer EXPR t Data
         )
      => SymbolMap' (a ': as) ctx (Expr t layout) where
    symbolMap' f expr = if (idx == eidx) then f sym else symbolMap' @as @ctx f expr where
        d    = unwrap' $ get @Data expr
        eidx = unwrap' $ get @Atom d
        idx  = fromIntegral $ natVal (Proxy :: Proxy idx)
        sym  = unsafeCoerce (unwrap' $ get @Sym d) :: ExprSymbol a (Expr t layout)

instance SymbolMap' '[] ctx expr where symbolMap' _ _ = impossible


-- === Symbol mapping === --

type  SymbolMap2 = SymbolMap2' (All Atom)
class SymbolMap2' (atoms :: [*]) ctx expr b where
    symbolMap2' :: (forall a. ctx a b => a -> b) -> expr -> b

symbolMap2 :: forall ctx expr b. SymbolMap2 ctx expr b => (forall a. ctx a b => a -> b) -> expr -> b
symbolMap2 = symbolMap2' @(All Atom) @ctx ; {-# INLINE symbolMap2 #-}

instance ( ctx (ExprSymbol a (Expr t layout)) b
         , SymbolMap2' as ctx (Expr t layout) b
         , idx ~ FromJust (Encode2 Atom a) -- FIXME: make it nicer
         , KnownNat idx, HasLayer EXPR t Data
         )
      => SymbolMap2' (a ': as) ctx (Expr t layout) b where
    symbolMap2' f expr = if (idx == eidx) then f sym else symbolMap2' @as @ctx f expr where
        d    = unwrap' $ get @Data expr
        eidx = unwrap' $ get @Atom d
        idx  = fromIntegral $ natVal (Proxy :: Proxy idx)
        sym  = unsafeCoerce (unwrap' $ get @Sym d) :: ExprSymbol a (Expr t layout)

instance SymbolMap2' '[] ctx expr b where symbolMap2' _ _ = impossible


-- type family FieldsType a
-- class HasFields a where
--     fieldList :: a -> [FieldsType a]

class HasFields2 a b where fieldList2 :: a -> b
instance (b ~ [FieldsType a], HasFields a) => HasFields2 a b where fieldList2 = fieldList

symbolFields :: SymbolMap2 HasFields2 expr b => expr -> b
symbolFields = symbolMap2 @HasFields2 fieldList2

-- WARNING: works only for Drafts for now as it assumes that the child-refs have the same type as the parent
type FieldsC t layout = SymbolMap2 HasFields2 (Expr t layout) [Ref (Link (Expr t layout) (Expr t layout))]
symbolFields2 :: (SymbolMap2 HasFields2 expr out, expr ~ Expr t layout, out ~ [Ref (Link expr expr)]) => expr -> out
symbolFields2 = symbolMap2 @HasFields2 fieldList2


-- === Instances === --

-- Show
instance {-# OVERLAPPABLE #-} Show (Unwrapped (AnyExpr t)) => Show (AnyExpr t       ) where show e = "Expr (" <> show (unwrap' e) <> ")" ; {-# INLINE show #-}
instance {-# OVERLAPPABLE #-} Show (AnyExpr t)             => Show (Expr    t layout) where show   = show . anyLayout                    ; {-# INLINE show #-}
instance {-# OVERLAPPABLE #-}                                 Show (AnyExpr I       ) where show   = impossible                          ; {-# INLINE show #-}

-- Properties

type instance Get p   (Expr t layout) = ExprGet p   (Expr t layout)
type instance Set p a (Expr t layout) = ExprSet p a (Expr t layout)

type family ExprGet p expr where
    ExprGet Layout (Expr _ layout) = layout
    ExprGet p      (Expr t layout) = Get p (Unwrapped (Expr t layout))

type family ExprSet p v expr where
    ExprSet Layout v (Expr t _)      = Expr t v
    ExprSet p      v (Expr t layout) = Expr t layout

instance (HasLayer' (Expr t layout) p, LayerData p (Expr t layout) ~ Get p (Expr t layout))
      => Getter p (Expr t layout) where get = view $ layer @p ; {-# INLINE get #-}

instance (Get p (Expr t layout) ~ LayerData p (Expr t layout), HasLayer EXPR t p)
      => Setter' p (Expr t layout) where set' el a = a & (layer @p) .~ el ; {-# INLINE set' #-}

-- Sub
type instance Sub s (Expr t layout) = Expr t (Sub s layout)


type instance LayerData Data (Expr t layout) = TermStore
instance Monad m => Constructor TermStore m (Layer (Expr t layout) Data) where cons = return . Layer

-- Scoping
instance {-# OVERLAPPABLE #-} (t ~ t', Generalize layout layout')                 => Generalize (Expr t layout) (Expr t' layout')
instance {-# OVERLAPPABLE #-} (a ~ Expr t' layout', Generalize (Expr t layout) a) => Generalize (Expr t layout)     a
instance {-# OVERLAPPABLE #-} (a ~ Expr t' layout', Generalize a (Expr t layout)) => Generalize a               (Expr t layout)

-- Repr
instance HasLayer EXPR t Data => Repr HeaderOnly (Expr t layout) where repr expr = symbolMap @(Repr HeaderOnly) repr expr

-- IsLayerStack
instance IsLayerStack (Expr t layout)

-- Universal
type instance Universal (Expr t _) = Expr t Draft


------- new things

type instance Cfg2 (Expr t layout) = t


class (Monad m, IsResult m, Inferable2 TermType t m) => TTT t m where
    elems'  :: m [Ref        (Expr t Draft) ]
    links'  :: m [Ref (Link' (Expr t Draft))]
    -- groups' :: m [Ref (Group (Expr t Draft))]

elemsM :: TTT t m => m [Ref (Expr t Draft) ]
elems  :: TTT t m => m [Ref (Expr t Draft) ]
elemsM = elems' ; {-# INLINE elemsM #-}
elems  = elems' ; {-# INLINE elems  #-}




instance {-# OVERLAPPING #-} Show (Ref (AnyExpr t))             => Show (Ref (Expr    t layout)) where show = show . anyLayout3 ; {-# INLINE show #-}
instance {-# OVERLAPPING #-}                                       Show (Ref (AnyExpr I))        where show = impossible        ; {-# INLINE show #-}
instance {-# OVERLAPPING #-} Show (Unwrapped (Ref (AnyExpr t))) => Show (Ref (AnyExpr t))        where show r = "Ref (" <> show (unwrap' r) <> ")" ; {-# INLINE show #-}






------------------------- something




-- specifyLayout2 :: Binding (Expr t Layout.Any) -> Binding (Expr t layout)
-- specifyLayout2 = unsafeCoerce

anyLayout :: Expr t layout -> Expr t Layout.Any
anyLayout = unsafeCoerce

-- anyLayout2 :: Binding (Expr t layout) -> Binding (Expr t Layout.Any)
-- anyLayout2 = unsafeCoerce

anyLayout3 :: Ref (Expr t layout) -> Ref (Expr t Layout.Any)
anyLayout3 = unsafeCoerce




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
type instance Decode rec Lam     = 11
type instance Decode rec Match   = 12
type instance Decode rec Missing = 13
type instance Decode rec Native  = 14
type instance Decode rec Star    = 15
type instance Decode rec Unify   = 16
type instance Decode rec Var     = 17


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
type instance DecodeComponent 11 = Lam
type instance DecodeComponent 12 = Match
type instance DecodeComponent 13 = Missing
type instance DecodeComponent 14 = Native
type instance DecodeComponent 15 = Star
type instance DecodeComponent 16 = Unify
type instance DecodeComponent 17 = Var


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
