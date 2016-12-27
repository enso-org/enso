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
import Luna.IR.Expr.Atom    (Atom, Atoms, AtomRep, atomRep, AtomOf)
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
import           Luna.IR.Expr.Term.Class   (InputsType, HasInputs, inputList)
import qualified Type.List                 as List
import qualified Data.Event                as Event
import           Data.Event                (Event(Event), Emitter, emit, (//), type (//))
import Luna.IR.Expr.Term.Uni ()
-- import Type.Inference
import Data.TypeVal
import Type.Bool (And)

import System.Log (MonadLogging, Logging, withDebugBy)



type EqPrimStates m n = (PrimState m ~ PrimState n)

class IsIdx t where
    idx :: Iso' t Int
    default idx :: (Wrapped t, Unwrapped t ~ Int) => Lens' t Int
    idx = wrapped' ; {-# INLINE idx #-}



data LAYER = LAYER deriving (Show)
data NET   = NET   deriving (Show)
data ATTR  = ATTR  deriving (Show)

-- data Net  t
-- data Attr t

--------------------
-- === Events === --
--------------------


data NEW    = NEW    deriving (Show)
data DELETE = DELETE deriving (Show)



------------------
-- === Elem === --
------------------

newtype Elem t = Elem Int deriving (Show, Ord, Eq)
makeWrapped '' Elem

type instance Definition (Elem t) = Definition t
type instance Abstract   (Elem t) = Abstract   t -- FIXME[WD]: to make everything more consistent, shouldnt we here return Elem (Abstract t) ?
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
-- === Keys === --
------------------

--- === Definition === --

-- newtype Key  s k = Key (KeyData s k)
newtype Key k a m = Key (KeyData k a m)

type family KeyData k a (m :: * -> *)

type family Keys k as m where
    Keys k '[]       m = '[]
    Keys k (a ': as) m = Key k a m ': Keys k as m

-- type        KeyData k a mey = KeyData (PrimState m) key

makeWrapped ''Key

type Key' k a m = Key k a (GetPassHandler m)
-- type RebasedKeyData k a m n = (KeyData k a n ~ KeyData k a m)
--
-- rebaseKey :: RebasedKeyData k a m n => Key k a m -> Key k a n
-- rebaseKey = rewrap ; {-# INLINE rebaseKey #-}


-- === Key Monad === --

-- class Monad m => KeyMonad key m n where
--     uncheckedLookupKey :: m (Maybe (Key n key))

class Monad m => KeyMonad k m where
    uncheckedLookupKey :: forall a. TypeRep {- the type of `a` -}
                       -> m (Maybe (Key' k a m))


-- === Construction === --

readKey :: forall k a m n. Monad m => Key k a n -> m (KeyData k a n)
readKey = return . unwrap' ; {-# INLINE readKey #-}

writeKey :: forall k a m n. Monad m => KeyData k a n -> m (Key k a n)
writeKey = return . wrap' ; {-# INLINE writeKey #-}


-- === Key access === --

-- FIXME: To refactor
-- Keys should be moved to Pass, because they internal monad ALWAYS defaults to pass
type family GetPassHandler (m :: * -> *) :: * -> *
type KeyData' k a m = KeyData k a (GetPassHandler m)





-- FIXME[WD]: Should we make PassMonad superclass of Reader / Writer?


type Editor k a m = (Reader k a m, Writer k a m)
type TransST    t m   = (MonadTrans t, Monad (t m), PrimState (t m) ~ PrimState m)

-- Reader
class    Monad m                                => Reader k a m     where getKey :: m (Key k a (GetPassHandler m))
instance {-# OVERLAPPABLE #-} SubReader k a t m => Reader k a (t m) where getKey = lift getKey ; {-# INLINE getKey #-}
type SubReader k a t m = (Reader k a m, TransST t m, GetPassHandler (t m) ~ GetPassHandler m)

-- Writer
class    Monad m                                => Writer k a m     where putKey :: Key k a (GetPassHandler m) -> m ()
instance {-# OVERLAPPABLE #-} SubWriter k a t m => Writer k a (t m) where putKey = lift . putKey ; {-# INLINE putKey #-}
type SubWriter k a t m = (Writer k a m, TransST t m, GetPassHandler (t m) ~ GetPassHandler m)


readComp :: forall k a m. Reader k a m => m (KeyData' k a m)
readComp = readKey =<< getKey @k @a ; {-# INLINE readComp #-}

writeComp :: forall k a m. Writer k a m => KeyData' k a m -> m ()
writeComp = putKey @k @a <=< writeKey ; {-# INLINE writeComp #-}


-- === Errors === --

type KeyAccessError action k a = Sentence $ ErrMsg "Key"
                                      :</>: 'ShowType k
                                      :</>: Parensed ('ShowType a)
                                      :</>: ErrMsg "is not"
                                      :</>: (ErrMsg action :<>: ErrMsg "able")

type KeyMissingError k = Sentence $ ErrMsg "Key"
                              :</>: Ticked ('ShowType k)
                              :</>: ErrMsg "is not accessible"

type KeyReadError  k a = KeyAccessError "read"  k a
type KeyWriteError k a = KeyAccessError "write" k a



---------------------
-- === IRStore === --
---------------------

-- === Definition === --

type LayerRep = TypeRep
type ElemRep  = TypeRep

type    IR     = IR'   ElemStore
type    IRST s = IR'  (ElemStoreST s)
type    IRM  m = IRST (PrimState   m)
newtype IR'  a = IR   (Map ElemRep a) deriving (Show, Default, Functor, Traversable, Foldable)

                            --  , _attrs :: Map LayerRep Any
                            --  }
                             --  , _genericLayers :: LayerConsStore m

-- data ElemStoreOld m = ElemStoreOld { _layerValues   :: ElemStoreM m
--                             --  , _elemLayers    :: LayerConsStore m
--                              }

type LayerSet    s = Store.VectorRef s Prim.Any
type ElemStore     = LayerStore      LayerRep Prim.Any
type ElemStoreST s = LayerStoreRef s LayerRep Prim.Any
type ElemStoreM  m = ElemStoreST (PrimState m)

type LayerConsStore m = Map LayerRep (AnyCons m)

-- makeLenses ''ElemStoreOld
makeWrapped ''IR'


-- === Accessors === --

-- specificLayers :: ElemRep -> Traversal' (IRM m) (LayerConsStore m)
-- specificLayers el = wrapped' . ix el . elemLayers ; {-# INLINE specificLayers #-}

-- emptyElemStoreOld :: PrimMonad m => m (ElemStoreOld m)
-- emptyElemStoreOld = ElemStoreOld <$> Store.empty -- <*> pure def ; {-# INLINE emptyElemStoreOld #-}


-- === Instances === --

-- instance Default (IRM m) where def = IRM def def

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

atElem :: Functor m => ElemRep -> (Maybe (ElemStoreM m) -> m (Maybe (ElemStoreM m))) -> IRM m -> m (IRM m)
atElem = wrapped' .: at  ; {-# INLINE atElem #-}

modifyElem  :: PrimMonad m => ElemRep -> (ElemStoreM m ->    ElemStoreM m)  -> IRM m -> m (IRM m)
modifyElemM :: PrimMonad m => ElemRep -> (ElemStoreM m -> m (ElemStoreM m)) -> IRM m -> m (IRM m)
modifyElem  e   = modifyElemM e . fmap return                                                  ; {-# INLINE modifyElem  #-}
modifyElemM e f = atElem e $ \es -> fmap Just $ f =<< fromMaybe (Store.empty) (fmap return es) ; {-# INLINE modifyElemM #-}


-- | The type `t` is not validated in any way, it is just constructed from index.
uncheckedElems :: forall t m. (MonadPass m, IsIdx t, Reader NET (Abstract t) m) => m [t]
uncheckedElems = fmap (view $ from idx) <$> (liftPassHandler . Store.ixes =<< readNet @(Abstract t)) ; {-# INLINE uncheckedElems #-}


-- === Construction === --


-- newMagicElem :: forall t m. (IRMonad m, KnownType (Abstract t), IsIdx t) => Definition t -> m t
-- newMagicElem tdef = do
--     irstate    <- getIR
--     let trep = typeVal' @(Abstract t)
--         Just layerStore = irstate ^? wrapped' . ix trep
--     newIdx <- Store.reserveIdx layerStore
--     let el = newIdx ^. from idx
--     return el
-- {-# INLINE newMagicElem #-}



type NewElemEvent2 m t = Emitter m (NEW // Abstract t)
newElem2 :: forall t m. ( MonadPass m, Editor NET (Abstract t) m, NewElemEvent2 m t, IsIdx t, KnownType (Abstract t))
        => Definition t -> m t
newElem2 tdef = do
    t <- reserveElem
    withDebugBy "Emitter" ("NEW // " <> show (typeVal' @(Abstract t) :: TypeRep) <> " [" <> show (t ^. idx) <> "]") $ do
        dispatchNewElem2 t tdef
    return t
{-# INLINE newElem2 #-}



type instance Event.Payload (NEW // t) = (t, Definition t)

type instance Abstract (a // b) = Abstract a // Abstract b
type instance Abstract NEW = NEW



reserveElem :: forall t m. (MonadPass m, Editor NET (Abstract t) m, IsIdx t) => m t
reserveElem = view (from idx) <$> reserveNewElemIdx @t ; {-# INLINE reserveElem #-}

dispatchNewElem2 :: forall t m. NewElemEvent2 m t => t -> Definition t -> m ()
dispatchNewElem2 t tdef = emit $ Event @(NEW // t) (t, tdef)


freeElem :: forall t m. (MonadPass m, IsIdx t, Editor NET (Abstract t) m) => t -> m ()
freeElem t = liftPassHandler . flip Store.freeIdx (t ^. idx) =<< readComp @NET @(Abstract t) ; {-# INLINE freeElem #-}

    -- FIXME[MK->WD]: Yes. It's an undefined. Un. De. Fi. Ned. You know what to do :P
    -- delete :: forall t m. (MonadPass m, IsIdx t, Editor NET (Abstract t) m, Event.Emitter m (DELETE // Abstract t), Event.Payload (DELETE // Abstract t) ~ (Universal t, Prim.Any))
    --        => t -> m ()
    -- delete t = emit (DELETE // abstract t) (universal t, undefined) >> freeElem t ; {-# INLINE delete #-}

reserveNewElemIdx :: forall t m. (MonadPass m, Editor NET (Abstract t) m) => m Int
reserveNewElemIdx = liftPassHandler . Store.reserveIdx =<< readComp @NET @(Abstract t) ; {-# INLINE reserveNewElemIdx #-}

readLayerByKey :: (IRMonad m, IsIdx t) => Key LAYER (Layer (Abstract t) layer) m -> t -> m (LayerData layer t)
readLayerByKey key t = unsafeCoerce <$> (Store.unsafeRead (t ^. idx) =<< readKey key) ; {-# INLINE readLayerByKey #-}

writeLayerByKey :: (IRMonad m, IsIdx t) => Key LAYER (Layer (Abstract t) layer) m -> LayerData layer t -> t -> m ()
writeLayerByKey key val t = (\v -> Store.unsafeWrite v (t ^. idx) $ unsafeCoerce val) =<< readKey key ; {-# INLINE writeLayerByKey #-}

readLayer :: forall layer t m. (MonadPass m, IsIdx t, Reader LAYER (Layer (Abstract t) layer) m) => t -> m (LayerData layer t)
readLayer t = liftPassHandler . flip readLayerByKey t =<< getKey @LAYER @(Layer (Abstract t) layer) ; {-# INLINE readLayer #-}

-- FIXME[WD]: writeLayer should need writable
writeLayer :: forall layer t m. (MonadPass m, IsIdx t, Reader LAYER (Layer (Abstract t) layer) m) => LayerData layer t -> t -> m ()
writeLayer val t = (\k -> liftPassHandler $ writeLayerByKey k val t) =<< getKey @LAYER @(Layer (Abstract t) layer) ; {-# INLINE writeLayer #-}

modifyLayerM :: forall layer t m a. (MonadPass m, IsIdx t, Reader LAYER (Layer (Abstract t) layer) m) => (LayerData layer t -> m (a, LayerData layer t)) -> t -> m a
modifyLayerM f t = do
    l      <- readLayer @layer t
    (a,l') <- f l
    writeLayer @layer l' t
    return a
{-# INLINE modifyLayerM #-}

modifyLayerM_ :: forall layer t m. (MonadPass m, IsIdx t, Reader LAYER (Layer (Abstract t) layer) m) => (LayerData layer t -> m (LayerData layer t)) -> t -> m ()
modifyLayerM_ = modifyLayerM @layer . (fmap.fmap) ((),) ; {-# INLINE modifyLayerM_ #-}

modifyLayer_ :: forall layer t m. (MonadPass m, IsIdx t, Reader LAYER (Layer (Abstract t) layer) m) => (LayerData layer t -> LayerData layer t) -> t -> m ()
modifyLayer_ = modifyLayerM_ @layer . fmap return ; {-# INLINE modifyLayer_ #-}


readAttr :: forall a m. Reader ATTR a m => m (KeyData' ATTR a m)
readAttr = readComp @ATTR @a ; {-# INLINE readAttr #-}

writeAttr :: forall a m. Writer ATTR a m => KeyData' ATTR a m -> m ()
writeAttr a = writeComp @ATTR @a a

readNet :: forall a m. (Reader NET a m) => m (KeyData' NET a m)
readNet = readComp @NET @a ; {-# INLINE readNet #-}


-- === Registration === --

registerElemWith :: forall el m. (KnownType el, IRMonad m) => (ElemStoreM m -> ElemStoreM m) -> m ()
registerElemWith = modifyIRM_ . modifyElem (typeVal' @el) ; {-# INLINE registerElemWith #-}

registerElem :: forall el m. (KnownType el, IRMonad m) => m ()
registerElem = registerElemWith @el id ; {-# INLINE registerElem #-}

attachLayerIR :: IRMonad m => LayerRep -> ElemRep -> m ()
attachLayerIR l e = do
    s <- getIR
    let Just estore = s ^? wrapped' . ix e -- Internal error if not found (element not registered)
    Store.unsafeAddKey l estore
{-# INLINE attachLayerIR #-}


-- === Instances === --

instance MonadLogging m => MonadLogging (IRBuilder m)


----------------------
-- === IRMonad === ---
----------------------

-- === Definition === --

-- | IRMonad is subclass of MonadFix because many expr operations reuire recursive calls.
--   It is more convenient to store it as global constraint, so it could be altered easily in the future.
type  IRMonadBase   m = (PrimMonad   m, MonadFix m, Logging m, MonadIO m) -- FIXME[WD]: remove io
type  IRMonadBaseIO m = (IRMonadBase m, MonadIO  m)
class IRMonadBase m => IRMonad m where
    getIR :: m (IRM m)
    putIR :: IRM m -> m ()

instance {-# OVERLAPPABLE #-} (IRMonadBase m) => IRMonad (IRBuilder m) where
    getIR = wrap'   State.get ; {-# INLINE getIR #-}
    putIR = wrap' . State.put ; {-# INLINE putIR #-}

instance {-# OVERLAPPABLE #-} IRMonadTrans t m => IRMonad (t m) where
    getIR = lift   getIR ; {-# INLINE getIR #-}
    putIR = lift . putIR ; {-# INLINE putIR #-}

type IRMonadTrans t m = (IRMonad m, MonadTrans t, IRMonadBase (t m), PrimState (t m) ~ PrimState m)



--FIXME: REFACTOR
type MonadPassInvariants m = (PrimState m ~ PrimState (GetPassHandler m))
type MonadPassBase       m = (IRMonad m, PrimMonad m)
type MonadPassCtx        m = (MonadPassBase m, MonadPassBase (GetPassHandler m), MonadPassInvariants m)
class MonadPassCtx m => MonadPass m where
    liftPassHandler :: forall a. GetPassHandler m a -> m a


-- === Modyfication === --

modifyIRM :: IRMonad m => (IRM m -> m (a, IRM m)) -> m a
modifyIRM f = do
    s <- getIR
    (a, s') <- f s
    putIR s'
    return a
{-# INLINE modifyIRM #-}

modifyIRM_ :: IRMonad m => (IRM m -> m (IRM m)) -> m ()
modifyIRM_ = modifyIRM . fmap (fmap ((),)) ; {-# INLINE modifyIRM_ #-}

modifyIR_ :: IRMonad m => (IRM m -> IRM m) -> m ()
modifyIR_ = modifyIRM_ . fmap return ; {-# INLINE modifyIR_ #-}

snapshot :: IRMonad m => m IR
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
-- === Key types === --
-----------------------

-- === Definitions === --

type instance KeyData LAYER _ m = LayerSet    (PrimState m)
type instance KeyData NET   _ m = ElemStoreST (PrimState m)


-- === Aliases === --

newtype AttrRep = AttrRep TypeRep deriving (Show, Eq, Ord)
instance IsTypeRep AttrRep
makeWrapped '' AttrRep

-- === Instances === --


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

link :: forall a b m. (Show a, Show b, MonadPass m, KnownType (Abstract (Link a b)), NewElemEvent2 m (Link a b), Editor NET (Abstract (Link a b)) m)
     => a -> b -> m (Link a b)
link a b = newElem2 (a,b) ; {-# INLINE link #-}


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

group :: forall f a m. (Show a, MonadPass m, Foldable f, Ord a, NewElemEvent2 m (Group a), KnownType (Abstract (Group a)), Editor NET (Abstract (Group a)) m)
      => f a -> m (Group a)
group = newElem2 . foldl' (flip Set.insert) mempty ; {-# INLINE group #-}




---------------------

data EXPR = EXPR deriving (Show)


------------------------
-- === ExprTerm === --
------------------------

data TMP -- FIXME

type    ExprTermDef atom t = N.Term atom (Layout.Named (SubLink NAME t) (SubLink EXPR t))
newtype ExprTerm    atom t = ExprTerm    (ExprTermDef atom t)
newtype ExprUniTerm      t = ExprUniTerm (N.UniTerm   (Layout.Named (SubLink NAME t) (SubLink EXPR t)))
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

data EXPRESSION layout
type instance Definition (EXPRESSION _) = ExprStore
type instance Abstract   (EXPRESSION _) = EXPR
type instance Universal  (EXPRESSION _) = EXPRESSION Layout.Any


-- === Definition === --

type Expr layout = Elem (EXPRESSION layout)
type AnyExpr     = Expr Layout.Any
type AnyExprLink = Link' AnyExpr


-- === Utils === --

unsafeRelayout :: Expr l -> Expr l'
unsafeRelayout = unsafeCoerce ; {-# INLINE unsafeRelayout #-}

expr2 :: forall atom layout m. (TermEncoder atom, MonadPass m, Editor NET EXPR m, Emitter m (NEW // EXPR))
      => ExprTerm atom (Expr layout) -> m (Expr layout)
expr2 = newElem2 . encodeTerm ; {-# INLINE expr2 #-}

reserveExpr :: (MonadPass m, Editor NET EXPR m)
            => m (Expr layout)
reserveExpr = reserveElem ; {-# INLINE reserveExpr #-}

dispatchNewExpr2 :: (Emitter m (NEW // EXPR), TermEncoder atom) => ExprTerm atom (Expr layout) -> Expr layout -> m ()
dispatchNewExpr2 = flip dispatchNewElem2 . encodeTerm


exprs :: (MonadPass m, Reader NET EXPR m) => m [AnyExpr]
exprs = uncheckedElems ; {-# INLINE exprs #-}

links :: (MonadPass m, Reader NET (LINK' EXPR) m) => m [AnyExprLink]
links = uncheckedElems ; {-# INLINE links #-}


-- | Expr pattern matching utility
match :: (MonadPass m, Reader LAYER (Layer EXPR Model) m)
      => Expr layout -> (Unwrapped (ExprUniTerm (Expr layout)) -> m a) -> m a
match t f = f . unwrap' =<< (exprUniTerm t) ; {-# INLINE match #-}

-- | Term unification
exprUniTerm :: (MonadPass m, Reader LAYER (Layer EXPR Model) m) => Expr layout -> m (ExprUniTerm (Expr layout))
exprUniTerm t = ExprUniTerm <$> symbolMapM_AB @ToUniTerm toUniTerm t ; {-# INLINE exprUniTerm #-}

class ToUniTerm a b where toUniTerm :: a -> b
instance (Unwrapped a ~ Term t l, b ~ UniTerm l, IsUniTerm t l, Wrapped a)
      => ToUniTerm a b where toUniTerm = uniTerm . unwrap' ; {-# INLINE toUniTerm #-}


-- === Instances === --

--FIXME[MK->WD]: I don't care for the second part of the tuple, it's necessary to make pass manager magic work, but should be removed ASAP
type instance Event.Payload (DELETE // EXPR)       = (AnyExpr, Prim.Any)
type instance Event.Payload (DELETE // LINK' EXPR) = (Link' AnyExpr, Prim.Any)

type instance Sub s     (Expr l) = Expr (Sub s l)


type instance Generalizable (Expr l) (Expr l') = ExprGeneralizable l l'

type family ExprGeneralizable l l' where
    ExprGeneralizable l Layout.Any = 'True -- FIXME[WD]: shouldn't we introduce `Layoyut` newtype wrapper to indicate that layouts could be always generalized to Any?
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




type ExprLayer     = Layer EXPR
type ExprLinkLayer = Layer (LINK' EXPR)
-- type ExprNet       = Net   EXPR
-- type ExprLinkNet   = Net   (LINK' EXPR)
-- type ExprGroupNet  = Net   (GROUP EXPR)


type ExprLayers     ls = ExprLayer     <$> ls
type ExprLinkLayers ls = ExprLinkLayer <$> ls
-- type Nets           ls = Net           <$> ls

type Editors k as m = (Readers k as m, Writers k as m)

type family Readers k as m :: Constraint where
    Readers k '[]       m = ()
    Readers k (a ': as) m = (Reader k a m, Readers k as m)

type family Writers k as m :: Constraint where
    Writers k '[]       m = ()
    Writers k (a ': as) m = (Writer k a m, Writers k as m)



unsafeToExprTerm :: forall atom l m. (MonadPass m, Reader LAYER (ExprLayer Model) m) => Expr l -> m (ExprTerm atom (Expr l))
unsafeToExprTerm = unsafeCoerce . unwrap' . access @TERM . unwrap' <∘> readLayer @Model ; {-# INLINE unsafeToExprTerm #-}

unsafeModifyExprTermDef :: forall atom l m. (MonadPass m, Editor LAYER (ExprLayer Model) m)
                        => Expr l -> (ExprTermDef atom (Expr l) -> ExprTermDef atom (Expr l)) -> m ()
unsafeModifyExprTermDef expr f = do
    oldModel <- readLayer @Model expr
    let oldTerm :: ExprTerm atom (Expr l) = unsafeCoerce $ unwrap' $ access @TERM $ unwrap' oldModel
    let oldDef   = unwrap' oldTerm
    let newModel = wrap' $ update' @TERM (wrap' $ unsafeCoerce $ (wrap' $ f oldDef :: ExprTerm atom (Expr l))) $ unwrap' oldModel
    writeLayer @Model newModel expr

unsafeToExprTermDef :: forall atom l m. (MonadPass m, Reader LAYER (ExprLayer Model) m) => Expr l -> m (ExprTermDef atom (Expr l))
unsafeToExprTermDef = unwrap' <∘> unsafeToExprTerm ; {-# INLINE unsafeToExprTermDef #-}







-- === Term mapping === --
-- | General expr symbol mapping utility. It allows mapping over current symbol in any expr.

class    MonadPass m => TermMapM (atoms :: [*]) ctx expr m b where symbolMapM :: (forall a. ctx a m b => a -> m b) -> expr -> m b
instance MonadPass m => TermMapM '[]            ctx expr m b where symbolMapM _ _ = impossible
instance (  TermMapM as ctx expr m b
         , ctx (ExprTerm a expr) m b
         , idx ~ FromJust (Encode2 Atom a) -- FIXME: make it nicer and assert
         , KnownNat idx
         , Reader LAYER (Layer EXPR Model) m
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
getAtomRep :: (TermMapM_A HasAtom expr m out, expr ~ Expr layout, out ~ AtomRep) => expr -> m out
getAtomRep = symbolMapM_A @HasAtom atomRep

isSameAtom :: (TermMapM_A HasAtom expr m out, expr ~ Expr layout, out ~ AtomRep) => expr -> expr -> m Bool
isSameAtom a b = (==) <$> getAtomRep a <*> getAtomRep b

-- class Repr  s a        where repr  ::       a -> Builder s Tok


class ReprExpr a b where reprAnyExpr :: a -> b
instance (Repr s a, b ~ Builder s Tok) => ReprExpr a b where reprAnyExpr = repr

reprExpr :: (TermMapM_AB ReprExpr (Expr l) m out, out ~ Builder s Tok) => Expr l -> m out
reprExpr = symbolMapM_AB @ReprExpr reprAnyExpr



class    (ctx a b, Monad m) => DropMonad ctx a m b
instance (ctx a b, Monad m) => DropMonad ctx a m b

class    ctx a => FreeResult ctx a b
instance ctx a => FreeResult ctx a b
