{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE UndecidableInstances    #-}

module Luna.IR.Internal.IR where

import           Old.Data.Record              (Encode2)
import           Old.Data.Record.Model.Masked as X (VGRecord2, Store2(Store2), Slot(Slot), Enum(Enum))
import           Old.Data.Record.Model.Masked (encode2, EncodeStore, encodeStore, Mask, encodeNat, encodeData2, checkData2, decodeData2, Raw(Raw), unsafeRestore, decodeNat)

import           Luna.Prelude                 hiding (elem {- fix: -} , Enum, log)
import qualified Luna.Prelude as Prelude

import Control.Monad.State  (StateT, runStateT)
import Luna.IR.Internal.LayerStore (LayerStoreRef, LayerStoreRefM, LayerStore)
import Data.Map             (Map)
import Data.Property
import Data.RTuple          (TMap(..), empty, Assoc(..), Assocs, (:=:)) -- refactor empty to another library
import qualified GHC.Prim   as Prim
import Luna.IR.Layer
import Luna.IR.Layer.Model
import Luna.IR.Expr.Atom    (Atom, Atoms, AtomDesc, atomDescOf, AtomOf)
import qualified Luna.IR.Expr.Atom as A
import Luna.IR.Expr.Format  (Format, Draft)
import Luna.IR.Expr.Layout  (LAYOUT, LayoutOf, NAME, Generalizable, Universal, universal, Abstract, Sub, abstract)
import Luna.IR.Expr.Term    (TERM, Term, UncheckedFromTerm, FromTerm, UniTerm, IsUniTerm, uniTerm)
import Type.Bool            (And)
import Type.Container       (Every)
import Type.Container       (In)
import Type.Maybe           (FromJust)
import Type.Error
import Unsafe.Coerce        (unsafeCoerce)

import qualified Control.Monad.State       as State
import qualified Luna.IR.Internal.LayerStore as Store
import qualified Data.Map                  as Map
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import qualified Luna.IR.Expr.Layout       as Layout
import qualified Luna.IR.Expr.Term.Class   as N
import           Luna.IR.Expr.Term.Class   (InputsType, HasInputs, inputList, ModifiesFields, modifyFields)
import qualified Type.List                 as List
import qualified Data.Event                as Event
import           Data.Event                (Payload(Payload), Emitter, PayloadData, emit, type (//))
import Luna.IR.Expr.Term.Uni ()
-- import Type.Inference
import Data.TypeDesc
import Type.Bool (And)

import System.Log (MonadLogging, Logging, withDebugBy)
import           Control.Monad.Trans.Maybe (MaybeT)


type EqPrimStates m n = (PrimState m ~ PrimState n)

class IsIdx t where
    idx :: Iso' t Int
    default idx :: (Wrapped t, Unwrapped t ~ Int) => Lens' t Int
    idx = wrapped' ; {-# INLINE idx #-}



data Net   = Net   deriving (Show)
data Attr  = Attr  deriving (Show)

-- data Net  t
-- data Attr t

--------------------
-- === Events === --
--------------------


data New    = New    deriving (Show)
data Delete = Delete deriving (Show)
data Import = Import deriving (Show)



------------------
-- === Elem === --
------------------

data Element = Element deriving (Show)

newtype Elem t = Elem Int deriving (Show, Ord, Eq)
makeWrapped '' Elem

type instance Definition (Elem t) = Definition t
type instance Abstract   (Elem t) = Elem (Abstract  t)
type instance Universal  (Elem t) = Elem (Universal t)


-- === Classes === --

-- class ToElem a where
--     toElem :: a -> Elem
--     default toElem :: (Wrapped a, Unwrapped a ~ Elem) => a -> Elem
--     toElem = unwrap' ; {-# INLINE toElem #-}
--
-- class FromElem a where
--     fromElem :: Elem -> a
--     default fromElem :: (Wrapped a, Unwrapped a ~ Elem) => Elem -> a
--     fromElem = wrap' ; {-# INLINE fromElem #-}
--
-- type IsElem a = (ToElem a, FromElem a)
--
-- elem :: IsElem a => Iso' a Elem
-- elem = iso toElem fromElem ; {-# INLINE elem #-}


-- === Instances === --

instance IsIdx (Elem t) where
    idx = wrapped' ; {-# INLINE idx #-}


------------------
-- === Refs === --
------------------

--- === Definition === --

-- newtype Ref  s k = Ref (RefData s k)
newtype Ref k a m = Ref (RefData k a m)

type family RefData k a (m :: * -> *)

type family Refs k as m where
    Refs k '[]       m = '[]
    Refs k (a ': as) m = Ref k a m ': Refs k as m

-- type        RefData k a mey = RefData (PrimState m) key

makeWrapped ''Ref

type Ref' k a m = Ref k a (GetRefHandler m)
-- type RebasedRefData k a m n = (RefData k a n ~ RefData k a m)
--
-- rebaseRef :: RebasedRefData k a m n => Ref k a m -> Ref k a n
-- rebaseRef = rewrap ; {-# INLINE rebaseRef #-}


-- === Ref Monad === --

class Monad m => MonadRefLookup k m where
    uncheckedLookupRef :: forall a. TypeDesc {- the type of `a` -}
                       -> m (Maybe (Ref' k a m))


-- === Construction === --

-- readRef :: forall k a m n. Monad m => Ref k a n -> m (RefData k a n)
-- readRef = return . unwrap' ; {-# INLINE readRef #-}
--
-- writeRef :: forall k a m n. Monad m => RefData k a n -> m (Ref k a n)
-- writeRef = return . wrap' ; {-# INLINE writeRef #-}


-- === Ref access === --

-- FIXME: To refactor
-- Refs should be moved to Pass, because they internal monad ALWAYS defaults to pass
type family GetRefHandler (m :: * -> *) :: * -> *
type instance GetRefHandler (MaybeT m) = GetRefHandler m
type instance GetRefHandler (StateT s m) = GetRefHandler m

type RefData' k a m = RefData k a (GetRefHandler m)




-- MonadRefState
class Monad m => MonadRefState k a m where
    getRef :: m (Ref' k a m)
    putRef :: Ref' k a m -> m ()


type SubMonadRefState k a t m = (MonadRefState k a m, MonadTransInvariants' t m, GetRefHandler (t m) ~ GetRefHandler m)
instance {-# OVERLAPPABLE #-} SubMonadRefState k a t m
      => MonadRefState k a (t m) where
    getRef = lift   getRef ; {-# INLINE getRef #-}
    putRef = lift . putRef ; {-# INLINE putRef #-}


-- type Editor k a m = (Reader k a m, Writer k a m)
class    (Reader k a m, Writer k a m) => Editor k a m
instance (Reader k a m, Writer k a m) => Editor k a m
class MonadRefState k a m => Reader k a m
class MonadRefState k a m => Writer k a m

instance {-# OVERLAPPABLE #-} (Reader k a m, EqPrims m (t m), GetRefHandler (t m) ~ GetRefHandler m, Monad (t m), MonadTrans t) => Reader k a (t m)
instance {-# OVERLAPPABLE #-} (Reader k a m, EqPrims m (t m), GetRefHandler (t m) ~ GetRefHandler m, Monad (t m), MonadTrans t) => Writer k a (t m)


type family Readers k as m :: Constraint where
    Readers k '[]       m = ()
    Readers k (a ': as) m = (Reader k a m, Readers k as m)

type family Writers k as m :: Constraint where
    Writers k '[]       m = ()
    Writers k (a ': as) m = (Writer k a m, Writers k as m)



readComp :: forall k a m. MonadRefState k a m => m (RefData' k a m)
readComp = unwrap' <$> getRef @k @a ; {-# INLINE readComp #-}

writeComp :: forall k a m. MonadRefState k a m => RefData' k a m -> m ()
writeComp = putRef @k @a . wrap' ; {-# INLINE writeComp #-}


-- === Errors === --

type RefAccessError action k a = Sentence $ ErrMsg "Ref"
                                      :</>: 'ShowType k
                                      :</>: Parensed ('ShowType a)
                                      :</>: ErrMsg "is not"
                                      :</>: (ErrMsg action :<>: ErrMsg "able")

type RefMissingError k = Sentence $ ErrMsg "Ref"
                              :</>: Ticked ('ShowType k)
                              :</>: ErrMsg "is not accessible"

type RefReadError  k a = RefAccessError "read"  k a
type RefWriteError k a = RefAccessError "write" k a



---------------------
-- === IRStore === --
---------------------

-- === Definition === --

type LayerDesc = TypeDescT Layer
type ElemDesc  = TypeDescT Element


type    IR     = IR'   ElemStore
type    IRST s = IR'  (ElemStoreST s)
type    IRM  m = IRST (PrimState   m)
newtype IR'  a = IR   (Map ElemDesc a) deriving (Show, Default, Functor, Traversable, Foldable)



type LayerSet    s = Store.VectorRef s Prim.Any
type ElemStore     = LayerStore      LayerDesc Prim.Any
type ElemStoreST s = LayerStoreRef s LayerDesc Prim.Any
type ElemStoreM  m = ElemStoreST (PrimState m)

makeWrapped ''IR'


-- === Mutability === --

unsafeFreeze :: PrimMonad m => IRM m -> m IR
freeze       :: PrimMonad m => IRM m -> m IR
unsafeFreeze = mapM Store.unsafeFreeze ; {-# INLINE unsafeFreeze #-}
freeze       = mapM Store.freeze       ; {-# INLINE freeze       #-}

unsafeThaw :: PrimMonad m => IR -> m (IRM m)
thaw       :: PrimMonad m => IR -> m (IRM m)
unsafeThaw = mapM Store.unsafeThaw ; {-# INLINE unsafeThaw #-}
thaw       = mapM Store.thaw       ; {-# INLINE thaw       #-}


-----------------------
-- === IRBuilder === --
-----------------------

-- === Definition === --

newtype IRBuilder m a = IRBuilder (StateT (IRM m) m a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadThrow)
makeWrapped ''IRBuilder


-- === Accessors === --

atElem :: Functor m => ElemDesc -> (Maybe (ElemStoreM m) -> m (Maybe (ElemStoreM m))) -> IRM m -> m (IRM m)
atElem = wrapped' .: at  ; {-# INLINE atElem #-}

modifyElem  :: PrimMonad m => ElemDesc -> (ElemStoreM m ->    ElemStoreM m)  -> IRM m -> m (IRM m)
modifyElemM :: PrimMonad m => ElemDesc -> (ElemStoreM m -> m (ElemStoreM m)) -> IRM m -> m (IRM m)
modifyElem  e   = modifyElemM e . fmap return                                                  ; {-# INLINE modifyElem  #-}
modifyElemM e f = atElem e $ \es -> fmap Just $ f =<< fromMaybe (Store.empty) (fmap return es) ; {-# INLINE modifyElemM #-}


-- | The type `t` is not validated in any way, it is just constructed from index.
uncheckedElems :: forall t m. (MonadRef m, IsIdx t, Reader Net (Abstract t) m) => m [t]
uncheckedElems = fmap (view $ from idx) <$> (liftRefHandler . Store.ixes =<< readNet @(Abstract t)) ; {-# INLINE uncheckedElems #-}


-- === Construction === --


type NewElemEvent m t = (Emitter (New // Abstract t) m, Logging m, KnownType (Abstract t), IsIdx t)
newElem :: forall t m. ( MonadRef m, Writer Net (Abstract t) m, NewElemEvent m t)
        => Definition t -> m t
newElem tdef = do
    t <- reserveElem
    t <$ dispatchNewElem t tdef
{-# INLINE newElem #-}



type instance PayloadData (New    // t) = (t, Definition t)
type instance PayloadData (Delete // t) = t
type instance PayloadData (Import // t) = t

type instance Abstract (a // b) = Abstract a // Abstract b
type instance Abstract New    = New
type instance Abstract Delete = Delete
type instance Abstract Import = Import



reserveElem :: forall t m. (MonadRef m, Writer Net (Abstract t) m, IsIdx t) => m t
reserveElem = view (from idx) <$> reserveNewElemIdx @t ; {-# INLINE reserveElem #-}

dispatchNewElem :: forall t m. NewElemEvent m t => t -> Definition t -> m ()
dispatchNewElem t tdef = withDebugBy "Emitter" ("Event New // " <> show (getTypeDesc_ @(Abstract t)) <> " [" <> show (t ^. idx) <> "]")
                       $ emit $ Payload @(New // t) (t, tdef)
{-# INLINE dispatchNewElem #-}


freeElem :: forall t m. (MonadRef m, IsIdx t, Writer Net (Abstract t) m) => t -> m ()
freeElem t = liftRefHandler . flip Store.freeIdx (t ^. idx) =<< readComp @Net @(Abstract t) ; {-# INLINE freeElem #-}

delete :: forall t m. (MonadRef m, IsIdx t, Editor Net (Abstract t) m, Event.Emitter (Delete // Abstract t) m)
       => t -> m ()
delete t = emit (Payload @(Delete // t) t) >> freeElem t ; {-# INLINE delete #-}

reserveNewElemIdx :: forall t m. (MonadRef m, Writer Net (Abstract t) m) => m Int
reserveNewElemIdx = liftRefHandler . Store.reserveIdx =<< readComp @Net @(Abstract t) ; {-# INLINE reserveNewElemIdx #-}

readLayerByRef :: (MonadIR m, IsIdx t) => Ref Layer (Abstract t // layer) m -> t -> m (LayerData layer t)
readLayerByRef key t = unsafeCoerce <$> (Store.unsafeRead (t ^. idx) $ unwrap' key) ; {-# INLINE readLayerByRef #-}

writeLayerByRef :: (MonadIR m, IsIdx t) => Ref Layer (Abstract t // layer) m -> LayerData layer t -> t -> m ()
writeLayerByRef key val t = (\v -> Store.unsafeWrite v (t ^. idx) $ unsafeCoerce val) $ unwrap' key ; {-# INLINE writeLayerByRef #-}

readLayer :: forall layer t m. (MonadRef m, IsIdx t, Reader Layer (Abstract t // layer) m) => t -> m (LayerData layer t)
readLayer t = liftRefHandler . flip readLayerByRef t =<< getRef @Layer @(Abstract t // layer) ; {-# INLINE readLayer #-}

writeLayer :: forall layer t m. (MonadRef m, IsIdx t, Writer Layer (Abstract t // layer) m) => LayerData layer t -> t -> m ()
writeLayer val t = (\k -> liftRefHandler $ writeLayerByRef k val t) =<< getRef @Layer @(Abstract t // layer) ; {-# INLINE writeLayer #-}

modifyLayerM :: forall layer t m a. (MonadRef m, IsIdx t, Editor Layer (Abstract t // layer) m) => (LayerData layer t -> m (a, LayerData layer t)) -> t -> m a
modifyLayerM f t = do
    l      <- readLayer @layer t
    (a,l') <- f l
    writeLayer @layer l' t
    return a
{-# INLINE modifyLayerM #-}

modifyLayerM_ :: forall layer t m. (MonadRef m, IsIdx t, Editor Layer (Abstract t // layer) m) => (LayerData layer t -> m (LayerData layer t)) -> t -> m ()
modifyLayerM_ = modifyLayerM @layer . (fmap.fmap) ((),) ; {-# INLINE modifyLayerM_ #-}

modifyLayer_ :: forall layer t m. (MonadRef m, IsIdx t, Editor Layer (Abstract t // layer) m) => (LayerData layer t -> LayerData layer t) -> t -> m ()
modifyLayer_ = modifyLayerM_ @layer . fmap return ; {-# INLINE modifyLayer_ #-}


readAttr :: forall a m. Reader Attr a m => m (RefData' Attr a m)
readAttr = readComp @Attr @a ; {-# INLINE readAttr #-}

writeAttr :: forall a m. Writer Attr a m => RefData' Attr a m -> m ()
writeAttr a = writeComp @Attr @a a

readNet :: forall a m. Reader Net a m => m (RefData' Net a m)
readNet = readComp @Net @a ; {-# INLINE readNet #-}


-- === Registration === --

registerElemWith :: forall el m. (KnownType el, MonadIR m) => (ElemStoreM m -> ElemStoreM m) -> m ()
registerElemWith = modifyIRM_ . modifyElem (getTypeDesc @el) ; {-# INLINE registerElemWith #-}

registerElem :: forall el m. (KnownType el, MonadIR m) => m ()
registerElem = registerElemWith @el id ; {-# INLINE registerElem #-}

unsafeCreateNewLayer :: MonadIR m => LayerDesc -> ElemDesc -> m ()
unsafeCreateNewLayer l e = do
    s <- getIR
    let Just estore = s ^? wrapped' . ix e -- FIXME[WD]: Internal error if not found (element not registered)
    Store.unsafeAddKey l estore
{-# INLINE unsafeCreateNewLayer #-}


-- === Instances === --

instance MonadLogging m => MonadLogging (IRBuilder m)



----------------------
-- === MonadIR === ---
----------------------

-- === Definition === --

-- | MonadIR is subclass of MonadFix because many expr operations reuire recursive calls.
--   It is more convenient to store it as global constraint, so it could be altered easily in the future.
type  MonadIRBase   m = (PrimMonad   m, MonadFix m, Logging m, MonadIO m) -- FIXME[WD]: remove io
type  MonadIRBaseIO m = (MonadIRBase m, MonadIO  m)
class MonadIRBase m => MonadIR m where
    getIR :: m (IRM m)
    putIR :: IRM m -> m ()

instance {-# OVERLAPPABLE #-} (MonadIRBase m) => MonadIR (IRBuilder m) where
    getIR = wrap'   State.get ; {-# INLINE getIR #-}
    putIR = wrap' . State.put ; {-# INLINE putIR #-}

instance {-# OVERLAPPABLE #-} MonadIRTrans t m => MonadIR (t m) where
    getIR = lift   getIR ; {-# INLINE getIR #-}
    putIR = lift . putIR ; {-# INLINE putIR #-}

type MonadIRTrans t m = (MonadIR m, MonadTrans t, MonadIRBase (t m), PrimState (t m) ~ PrimState m)



--FIXME: REFACTOR
type RefHandlerInvariants m = (PrimState m ~ PrimState (GetRefHandler m))
type RefHandlerBase       m = (MonadIR m, PrimMonad m)
type RefHandlerCtx        m = (RefHandlerBase m, RefHandlerBase (GetRefHandler m), RefHandlerInvariants m)
class RefHandlerCtx m => MonadRef m where
    liftRefHandler :: forall a. GetRefHandler m a -> m a

instance {-# OVERLAPPABLE #-} (MonadTransInvariants' t m, GetRefHandler (t m) ~ GetRefHandler m, RefHandlerCtx (t m), MonadRef m)
      => MonadRef (t m) where
    liftRefHandler = lift . liftRefHandler

-- === Modyfication === --

modifyIRM :: MonadIR m => (IRM m -> m (a, IRM m)) -> m a
modifyIRM f = do
    s <- getIR
    (a, s') <- f s
    putIR s'
    return a
{-# INLINE modifyIRM #-}

modifyIRM_ :: MonadIR m => (IRM m -> m (IRM m)) -> m ()
modifyIRM_ = modifyIRM . fmap (fmap ((),)) ; {-# INLINE modifyIRM_ #-}

modifyIR_ :: MonadIR m => (IRM m -> IRM m) -> m ()
modifyIR_ = modifyIRM_ . fmap return ; {-# INLINE modifyIR_ #-}

snapshot :: MonadIR m => m IR
snapshot = freeze =<< getIR ; {-# INLINE snapshot #-}


-- === Running === --

evalIRBuilderM :: Monad m => IRBuilder m a -> IRM m -> m a
evalIRBuilderM = State.evalStateT . unwrap' ; {-# INLINE evalIRBuilderM #-}

evalIRBuilder :: PrimMonad m => IRBuilder m a -> IR -> m a
evalIRBuilder m = evalIRBuilderM m <=< thaw ; {-# INLINE evalIRBuilder #-}

evalIRBuilder' :: Monad m => IRBuilder m a -> m a
evalIRBuilder' = flip evalIRBuilderM def ; {-# INLINE evalIRBuilder' #-}


-- === Instances === --

instance MonadTrans IRBuilder where
    lift = wrap' . lift ; {-# INLINE lift #-}

instance PrimMonad m => PrimMonad (IRBuilder m) where
    type PrimState (IRBuilder m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}


-----------------------
-- === Ref types === --
-----------------------

-- === Definitions === --

type instance RefData Layer _ m = LayerSet    (PrimState m)
type instance RefData Net   _ m = ElemStoreST (PrimState m)


-- === Aliases === --

newtype AttrRep = AttrRep TypeDesc deriving (Show, Eq, Ord)
instance IsTypeDesc AttrRep
makeWrapped '' AttrRep



-------------------
-- === Link === --
-------------------

-- === Abstract === --

data LINK  a b = LINK a b deriving (Show)
type LINK' a   = LINK a a
type instance Definition (LINK a b) = (a,b)
type instance Abstract   (LINK a b) = LINK (Abstract  a) (Abstract  b)
type instance Universal  (LINK a b) = LINK (Universal a) (Universal b)


-- === Elem === --

type Link  a b = Elem (LINK a b)
type Link' a   = Link a a

type SubLink s t = Link (Sub s t) t



-- === Construction === --

link :: forall a b m. (Show a, Show b, MonadRef m, KnownType (Abstract (Link a b)), NewElemEvent m (Link a b), Writer Net (Abstract (Link a b)) m)
     => a -> b -> m (Link a b)
link a b = newElem (a,b) ; {-# INLINE link #-}


-- === Instances === ---

type instance Generalizable (Link a b) (Link a' b') = Generalizable a a' `And` Generalizable b b'



-------------------
-- === Group === --
-------------------

-- === Abstract === --

data GROUP a = Group a deriving (Show)
type instance Definition (GROUP a) = Set a
type instance Abstract   (GROUP a) = GROUP (Abstract  a)
type instance Universal  (GROUP a) = GROUP (Universal a)

type Group a = Elem (GROUP a)


-- === Construction === --

group :: forall f a m. (Show a, MonadRef m, Foldable f, Ord a, NewElemEvent m (Group a), KnownType (Abstract (Group a)), Writer Net (Abstract (Group a)) m)
      => f a -> m (Group a)
group = newElem . foldl' (flip Set.insert) mempty ; {-# INLINE group #-}




---------------------

-- data EXPR = EXPR deriving (Show)

data ANY

data EXPR layout
------------------------
-- === ExprTerm === --
------------------------

data TMP -- FIXME

type    ExprTermDef atom t = N.Term atom (Layout.Named (SubLink NAME t) (SubLink (Elem (EXPR ANY)) t))
newtype ExprTerm    atom t = ExprTerm    (ExprTermDef atom t)
newtype ExprUniTerm      t = ExprUniTerm (N.UniTerm   (Layout.Named (SubLink NAME t) (SubLink (Elem (EXPR ANY)) t)))
type    ExprTerm'   atom   = ExprTerm atom TMP
makeWrapped ''ExprTerm
makeWrapped ''ExprUniTerm


-- === Helpers === --

hideLayout :: ExprTerm atom t -> ExprTerm atom TMP
hideLayout = unsafeCoerce ; {-# INLINE hideLayout #-}


-- === Layout validation === ---
-- | Layout validation. Type-assertion utility, proving that symbol construction is not ill-typed.

type InvalidFormat sel a format = 'ShowType sel
                             :</>: Ticked ('ShowType a)
                             :</>: ErrMsg "is not a valid"
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
type ValidateLayout' t     sel a = ValidateLayout (t # LAYOUT) sel a


-- === Instances === --

-- FIXME: [WD]: it seems that LAYOUT in the below declaration is something else than real layout - check it and refactor
type instance Access LAYOUT (ExprTerm atom t) = Access LAYOUT (Unwrapped (ExprTerm atom t))
type instance Access Atom   (ExprTerm atom t) = atom
type instance Access Format (ExprTerm atom t) = Access Format atom
type instance Access TERM   (ExprTerm atom t) = ExprTerm atom t

instance Accessor TERM (ExprTerm atom t) where access = id ; {-# INLINE access #-}

instance UncheckedFromTerm (ExprTerm atom t) where uncheckedFromTerm = wrap' ; {-# INLINE uncheckedFromTerm #-}

instance ValidateLayout (LayoutOf t) Atom atom
      => FromTerm (ExprTerm atom t) where fromTerm = wrap' ; {-# INLINE fromTerm #-}


-- Repr
instance Repr s (Unwrapped (ExprTerm atom t))
      => Repr s (ExprTerm atom t) where repr = repr . unwrap' ; {-# INLINE repr #-}

-- Fields
type instance FieldsType (ExprTerm atom t) = FieldsType (Unwrapped (ExprTerm atom t))
instance HasFields (Unwrapped (ExprTerm atom t))
      => HasFields (ExprTerm atom t) where fieldList = fieldList . unwrap' ; {-# INLINE fieldList #-}

-- ModifyFields

instance ModifiesFields (Unwrapped (ExprTerm atom t))
      => ModifiesFields (ExprTerm atom t) where modifyFields f = wrap' . modifyFields f . unwrap' ; {-# INLINE modifyFields #-}


-- Inputs
type instance InputsType (ExprTerm atom t) = InputsType (Unwrapped (ExprTerm atom t))
instance HasInputs (Unwrapped (ExprTerm atom t))
      => HasInputs (ExprTerm atom t) where inputList = inputList . unwrap' ; {-# INLINE inputList #-}

-- AtomOf
type instance AtomOf (ExprTerm atom t) = AtomOf (Unwrapped (ExprTerm atom t))

----------------------
-- === ExprData === --
----------------------

type ExprStoreSlots = '[ Atom ':= Enum, Format ':= Mask, TERM ':= Raw ]
type ExprStore = Store2 ExprStoreSlots

newtype ExprData sys model = ExprData ExprStore deriving (Show)
makeWrapped ''ExprData


-- === Encoding === --

class                                                            TermEncoder atom where encodeTerm :: forall t. ExprTerm atom t -> ExprStore
instance                                                         TermEncoder I    where encodeTerm = impossible
instance EncodeStore ExprStoreSlots (ExprTerm' atom) Identity => TermEncoder atom where
    encodeTerm = runIdentity . encodeStore . hideLayout ; {-# INLINE encodeTerm #-} -- magic


------------------
-- === Expr === --
------------------


type instance Definition (EXPR _) = ExprStore
type instance Abstract   (EXPR _) = EXPR ANY
type instance Universal  (EXPR _) = EXPR Layout.Bottom


-- === Definition === --

type Expr layout   = Elem (EXPR layout)
type AnyExpr       = Expr ANY
type SomeExpr      = Expr Layout.Bottom
type SomeExprLink  = Link' SomeExpr
type AnyExprLink   = Link' AnyExpr
type ExprLink l l' = Link (Expr l) (Expr l')
type ExprLink'  l  = ExprLink l l


-- === Utils === --

unsafeRelayout :: Expr l -> Expr l'
unsafeRelayout = unsafeCoerce ; {-# INLINE unsafeRelayout #-}

expr :: forall atom layout m. (TermEncoder atom, MonadRef m, Writer Net AnyExpr m, Emitter (New // AnyExpr) m)
     => ExprTerm atom (Expr layout) -> m (Expr layout)
expr = newElem . encodeTerm ; {-# INLINE expr #-}

reserveExpr :: (MonadRef m, Writer Net AnyExpr m)
            => m (Expr layout)
reserveExpr = reserveElem ; {-# INLINE reserveExpr #-}

dispatchNewExpr :: (NewElemEvent m (Expr layout), TermEncoder atom) => ExprTerm atom (Expr layout) -> Expr layout -> m ()
dispatchNewExpr = flip dispatchNewElem . encodeTerm

exprs :: (MonadRef m, Reader Net AnyExpr m) => m [SomeExpr]
exprs = uncheckedElems ; {-# INLINE exprs #-}

links :: (MonadRef m, Reader Net (Link' AnyExpr) m) => m [SomeExprLink]
links = uncheckedElems ; {-# INLINE links #-}


-- | Expr pattern matching utility
match :: (MonadRef m, Reader Layer (AnyExpr // Model) m)
      => Expr layout -> (Unwrapped (ExprUniTerm (Expr layout)) -> m a) -> m a
match t f = f . unwrap' =<< (exprUniTerm t) ; {-# INLINE match #-}

-- | Term unification
exprUniTerm :: (MonadRef m, Reader Layer (AnyExpr // Model) m) => Expr layout -> m (ExprUniTerm (Expr layout))
exprUniTerm t = ExprUniTerm <$> symbolMapM_AB @ToUniTerm toUniTerm t ; {-# INLINE exprUniTerm #-}

class ToUniTerm a b where toUniTerm :: a -> b
instance (Unwrapped a ~ Term t l, b ~ UniTerm l, IsUniTerm t l, Wrapped a)
      => ToUniTerm a b where toUniTerm = uniTerm . unwrap' ; {-# INLINE toUniTerm #-}


-- === Instances === --

type instance Sub s     (Expr l) = Expr (Sub s l)


type instance Generalizable (Expr l) (Expr l') = ExprGeneralizable l l'

type family ExprGeneralizable l l' where
    ExprGeneralizable l Layout.Bottom = 'True -- FIXME[WD]: shouldn't we introduce `Layoyut` newtype wrapper to indicate that layouts could be always generalized to Any?
    ExprGeneralizable l l'         = Generalizable l l'


-- -------------------------------------
-- === Expr Layout type caches === --
-------------------------------------

type instance Encode2 Atom    v = List.Index v (Every Atom)
type instance Encode2 Format  v = List.Index v (Every Format)




-- TO REFACTOR:

type instance UnsafeGeneralizable (Expr l) (Expr l') = ()
type instance UnsafeGeneralizable (Link (Expr l) (Expr r)) (Link (Expr l') (Expr r')) = ()

type family         UnsafeGeneralizable a b :: Constraint
unsafeGeneralize :: UnsafeGeneralizable a b => a -> b
unsafeGeneralize = unsafeCoerce ; {-# INLINE unsafeGeneralize #-}




-- type ExprLayer     = Layer EXPR
-- type ExprLinkLayer = Layer (Link' AnyExpr)
-- type ExprNet       = Net   EXPR
-- type ExprLinkNet   = Net   (Link' AnyExpr)
-- type ExprGroupNet  = Net   (GROUP EXPR)


-- type ExprLayers     ls = ExprLayer     <$> ls
-- type ExprLinkLayers ls = ExprLinkLayer <$> ls
-- type Nets           ls = Net           <$> ls

type Editors k as m = (Readers k as m, Writers k as m)

type family MonadRefStates k as m :: Constraint where
    MonadRefStates k '[]       m = ()
    MonadRefStates k (a ': as) m = (MonadRefState k a m, MonadRefStates k as m)




unsafeToExprTerm :: forall atom l m. (MonadRef m, Reader Layer (AnyExpr // Model) m) => Expr l -> m (ExprTerm atom (Expr l))
unsafeToExprTerm = unsafeCoerce . unwrap' . access @TERM . unwrap' <∘> readLayer @Model ; {-# INLINE unsafeToExprTerm #-}

unsafeModifyExprTermDef :: forall atom l m. (MonadRef m, Editor Layer (AnyExpr // Model) m)
                        => Expr l -> (ExprTermDef atom (Expr l) -> ExprTermDef atom (Expr l)) -> m ()
unsafeModifyExprTermDef expr f = do
    oldModel <- readLayer @Model expr
    let oldTerm :: ExprTerm atom (Expr l) = unsafeCoerce $ unwrap' $ access @TERM $ unwrap' oldModel
    let oldDef   = unwrap' oldTerm
    let newModel = wrap' $ update' @TERM (wrap' $ unsafeCoerce $ (wrap' $ f oldDef :: ExprTerm atom (Expr l))) $ unwrap' oldModel
    writeLayer @Model newModel expr

unsafeToExprTermDef :: forall atom l m. (MonadRef m, Reader Layer (AnyExpr // Model) m) => Expr l -> m (ExprTermDef atom (Expr l))
unsafeToExprTermDef = unwrap' <∘> unsafeToExprTerm ; {-# INLINE unsafeToExprTermDef #-}







-- === Term mapping === --
-- | General expr symbol mapping utility. It allows mapping over current symbol in any expr.

class    MonadRef m => TermMapM (atoms :: [*]) ctx expr m b where symbolMapM :: (forall a. ctx a m b => a -> m b) -> expr -> m b
instance MonadRef m => TermMapM '[]            ctx expr m b where symbolMapM _ _ = impossible
instance (  TermMapM as ctx expr m b
         , ctx (ExprTerm a expr) m b
         , idx ~ FromJust (Encode2 Atom a) -- FIXME: make it nicer and assert
         , KnownNat idx
         , Reader Layer (AnyExpr // Model) m
         , expr ~ Expr layout
         )
      => TermMapM (a ': as) ctx expr m b where
    symbolMapM f expr = do
        d <- unwrap' <$> readLayer @Model expr
        sym <- unsafeToExprTerm @a expr
        let eidx = unwrap' $ access @Atom d
            idx  = fromIntegral $ natVal (Proxy :: Proxy idx)
        if (idx == eidx) then f sym else symbolMapM @as @ctx f expr
    {-# INLINE symbolMapM #-}


type TermMapM_AMB          = TermMapM     (Every Atom)
type TermMapM_AB  ctx      = TermMapM_AMB (DropMonad ctx)
type TermMap_AB   ctx expr = TermMapM_AB  ctx expr Identity
type TermMapM_A   ctx      = TermMapM_AB  (FreeResult ctx)
type TermMap_A    ctx expr = TermMapM_A   ctx expr Identity

symbolMapM_AMB :: forall ctx m expr b. TermMapM_AMB ctx expr m b => (forall a. ctx a m b => a -> m b) -> expr -> m b
symbolMapM_AB  :: forall ctx expr m b. TermMapM_AB  ctx expr m b => (forall a. ctx a   b => a ->   b) -> expr -> m b
symbolMap_AB   :: forall ctx expr   b. TermMap_AB   ctx expr   b => (forall a. ctx a   b => a ->   b) -> expr ->   b
symbolMapM_A   :: forall ctx expr m b. TermMapM_A   ctx expr m b => (forall a. ctx a     => a ->   b) -> expr -> m b
symbolMap_A    :: forall ctx expr   b. TermMap_A    ctx expr   b => (forall a. ctx a     => a ->   b) -> expr ->   b
symbolMapM_AMB   = symbolMapM @(Every Atom) @ctx                  ; {-# INLINE symbolMapM_AMB #-}
symbolMapM_AB  f = symbolMapM_AMB @(DropMonad ctx) (return <$> f) ; {-# INLINE symbolMapM_AB  #-}
symbolMap_AB   f = runIdentity . symbolMapM_AB @ctx f             ; {-# INLINE symbolMap_AB   #-}
symbolMapM_A     = symbolMapM_AB @(FreeResult ctx)                ; {-# INLINE symbolMapM_A   #-}
symbolMap_A    f = runIdentity . symbolMapM_A @ctx f              ; {-# INLINE symbolMap_A    #-}


class ModifiesFields2 a b where
    modifyFields2 :: (Link' SomeExpr -> Link' SomeExpr) -> a -> b

instance (ModifiesFields a, FieldsType a ~ Link' SomeExpr) => ModifiesFields2 a Raw where
    modifyFields2 f = wrap' . unsafeCoerce . modifyFields f

withFields :: (TermMapM_AB ModifiesFields2 expr m Raw, expr ~ Expr layout) => (Link' SomeExpr -> Link' SomeExpr) -> expr -> m Raw
withFields f = symbolMapM_AB @ModifiesFields2 (modifyFields2 f)

inplaceModifyFieldsWith :: (Editor Layer (AnyExpr // Model) m, TermMapM_AB ModifiesFields2 expr m Raw, expr ~ Expr layout) => (Link' SomeExpr -> Link' SomeExpr) -> expr -> m ()
inplaceModifyFieldsWith f expr = do
    newModel <- withFields f expr
    modifyLayer_ @Model (wrap' . update' @TERM newModel . unwrap') expr


class    (b ~ [FieldsType a], HasFields a) => HasFields2 a b
instance (b ~ [FieldsType a], HasFields a) => HasFields2 a b

-- WARNING: works only for Drafts for now as it assumes that the child-refs have the same type as the parent
-- type FieldsC t layout = TermMap2 HasFields2 (Expr t layout) [Ref (Link (Expr t layout) (Expr t layout))]
symbolFields :: (TermMapM_AB HasFields2 expr m out, expr ~ Expr layout, out ~ [Link expr expr]) => expr -> m out
symbolFields = symbolMapM_AB @HasFields2 fieldList

class    (b ~ [InputsType a], HasInputs a) => HasInputs2 a b
instance (b ~ [InputsType a], HasInputs a) => HasInputs2 a b
inputs :: (TermMapM_AB HasInputs2 expr m out, expr ~ Expr layout, out ~ [Link expr expr]) => expr -> m out
inputs = symbolMapM_AB @HasInputs2 inputList

class    KnownType (AtomOf a) => HasAtom a
instance KnownType (AtomOf a) => HasAtom a
termAtomDesc :: (TermMapM_A HasAtom expr m out, expr ~ Expr layout, out ~ AtomDesc) => expr -> m out
termAtomDesc = symbolMapM_A @HasAtom atomDescOf

isSameAtom :: (TermMapM_A HasAtom expr m out, expr ~ Expr layout, out ~ AtomDesc) => expr -> expr -> m Bool
isSameAtom a b = (==) <$> termAtomDesc a <*> termAtomDesc b

-- class Repr  s a        where repr  ::       a -> Builder s Tok


class ReprExpr a b where reprSomeExpr :: a -> b
instance (Repr s a, b ~ Builder s Tok) => ReprExpr a b where reprSomeExpr = repr

reprExpr :: (TermMapM_AB ReprExpr (Expr l) m out, out ~ Builder s Tok) => Expr l -> m out
reprExpr = symbolMapM_AB @ReprExpr reprSomeExpr



class    (ctx a b, Monad m) => DropMonad ctx a m b
instance (ctx a b, Monad m) => DropMonad ctx a m b

class    ctx a => FreeResult ctx a b
instance ctx a => FreeResult ctx a b



instance {-# OVERLAPPABLE #-} TypePretty (Elem e) where
    formatType = id

instance {-# OVERLAPPABLE #-}
         TypePretty (EXPR l)   where formatType   = ("Expr" :)
instance TypePretty (EXPR ANY) where formatType _ = ["AnyExpr"]


instance {-# OVERLAPPABLE #-} TypePretty (LINK  a b)     where formatType       = ("Link" :)
instance {-# OVERLAPPABLE #-} TypePretty (LINK' a)       where formatType [a,_] = ["Link'" , a]
instance                      TypePretty (LINK' AnyExpr) where formatType _     = ["AnyExprLink"]



instance TypePretty ANY where
    formatType = ("Any" :)
