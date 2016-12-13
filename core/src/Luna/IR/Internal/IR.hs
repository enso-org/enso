{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE UndecidableInstances    #-}

module Luna.IR.Internal.IR where

import           Old.Data.Record              (Encode2)
import           Old.Data.Record.Model.Masked as X (VGRecord2, Store2(Store2), Slot(Slot), Enum(Enum))
import           Old.Data.Record.Model.Masked (encode2, EncodeStore, encodeStore, Mask, encodeNat, encodeData2, checkData2, decodeData2, Raw(Raw), unsafeRestore, decodeNat)

import           Luna.Prelude                 hiding (elem {- fix: -} , Enum)
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
import           Data.Event                (Event(Event), emit, (//), type (//))
import Luna.IR.Expr.Term.Uni ()
-- import Type.Inference
import Data.TypeVal





type EqPrimStates m n = (PrimState m ~ PrimState n)

class IsIdx t where
    idx :: Iso' t Int
    default idx :: (Wrapped t, Unwrapped t ~ Int) => Lens' t Int
    idx = wrapped' ; {-# INLINE idx #-}



--------------------
-- === Events === --
--------------------


data NEW = NEW deriving (Show)



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
newtype Key m k = Key (KeyData m k)

type family KeyData (m :: * -> *) key
-- type        KeyData m key = KeyData (PrimState m) key

makeWrapped ''Key

type RebasedKeyData m n k = (KeyData n k ~ KeyData m k)

rebaseKey :: RebasedKeyData m n k => Key m k -> Key n k
rebaseKey = rewrap ; {-# INLINE rebaseKey #-}


-- === Key Monad === --

class Monad m => KeyMonad key m n where
    uncheckedLookupKey :: m (Maybe (Key n key))


-- === Construction === --

readKey :: forall k m. Monad m => Key m k -> m (KeyData m k)
readKey = return . unwrap' ; {-# INLINE readKey #-}

writeKey :: forall k m. Monad m => KeyData m k -> m (Key m k)
writeKey = return . wrap' ; {-# INLINE writeKey #-}


-- === Key access === --

type Accessible k m = (Readable k m, Writable k m)
type TransST    t m = (MonadTrans t, Monad (t m), PrimState (t m) ~ PrimState m)

-- Readable
class    Monad m                                => Readable k m     where getKey :: m (Key m k)
instance {-# OVERLAPPABLE #-} SubReadable k t m => Readable k (t m) where getKey = lift (rebaseKey <$> getKey) ; {-# INLINE getKey #-}
type SubReadable k t m = (Readable k m, TransST t m, RebasedKeyData (t m) m k)

-- Writable
class    Monad m                                => Writable k m     where putKey :: Key m k -> m ()
instance {-# OVERLAPPABLE #-} SubWritable k t m => Writable k (t m) where putKey = lift . putKey . rebaseKey ; {-# INLINE putKey #-}
type SubWritable k t m = (Writable k m, TransST t m, RebasedKeyData (t m) m k)


readComp :: forall k m. Readable k m => m (KeyData m k)
readComp = readKey =<< getKey @k ; {-# INLINE readComp #-}

writeComp :: forall k m. Writable k m => KeyData m k -> m ()
writeComp = putKey @k <=< writeKey ; {-# INLINE writeComp #-}


-- === Errors === --

type KeyAccessError action k = Sentence $ ErrMsg "Key"
                                    :</>: Ticked ('ShowType k)
                                    :</>: ErrMsg "is not"
                                    :</>: (ErrMsg action :<>: ErrMsg "able")

type KeyMissingError k = Sentence $ ErrMsg "Key"
                              :</>: Ticked ('ShowType k)
                              :</>: ErrMsg "is not accessible"

type KeyReadError  k = KeyAccessError "read"  k
type KeyWriteError k = KeyAccessError "write" k



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

newtype IRBuilder m a = IRBuilder (StateT (IRM m) m a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)
makeWrapped ''IRBuilder


-- === Accessors === --

atElem :: Functor m => ElemRep -> (Maybe (ElemStoreM m) -> m (Maybe (ElemStoreM m))) -> IRM m -> m (IRM m)
atElem = wrapped' .: at  ; {-# INLINE atElem #-}

modifyElem  :: PrimMonad m => ElemRep -> (ElemStoreM m ->    ElemStoreM m)  -> IRM m -> m (IRM m)
modifyElemM :: PrimMonad m => ElemRep -> (ElemStoreM m -> m (ElemStoreM m)) -> IRM m -> m (IRM m)
modifyElem  e   = modifyElemM e . fmap return                                                  ; {-# INLINE modifyElem  #-}
modifyElemM e f = atElem e $ \es -> fmap Just $ f =<< fromMaybe (Store.empty) (fmap return es) ; {-# INLINE modifyElemM #-}


-- | The type `t` is not validated in any way, it is just constructed from index.
uncheckedElems :: forall t m. (IRMonad m, IsIdx t, Readable (Net (Abstract t)) m) => m [t]
uncheckedElems = fmap (view $ from idx) <$> (Store.ixes =<< readNet @(Abstract t)) ; {-# INLINE uncheckedElems #-}


-- === Querying === --
--
-- lookupGenericLayerCons :: LayerRep -> IRM m -> Maybe (AnyCons m)
-- lookupGenericLayerCons l s = s ^? genericLayers . ix l ; {-# INLINE lookupGenericLayerCons #-}
--
-- lookupSpecificLayerCons :: ElemRep -> LayerRep -> IRM m -> Maybe (AnyCons m)
-- lookupSpecificLayerCons el l s = s ^? specificLayers el . ix l ; {-# INLINE lookupSpecificLayerCons #-}
--
-- lookupLayerCons :: ElemRep -> LayerRep -> IRM m -> Maybe (AnyCons m)
-- lookupLayerCons el l s = lookupSpecificLayerCons el l s <|> lookupGenericLayerCons l s ; {-# INLINE lookupLayerCons #-}
--
-- lookupLayerCons' :: ElemRep -> LayerRep -> IRM m -> AnyCons m
-- lookupLayerCons' el l = fromMaybe (error $ "Fatal error " <> show el <> " " <> show l) . lookupLayerCons el l ; {-# INLINE lookupLayerCons' #-}


-- === Construction === --


-- to zalezne od layeru a tak nie moze chyba byc, bo chcem yto odpalic jako ala-event dla kazdego layeru danego elementu
-- class Monad m => Cons7 l m a where
--     cons7 :: forall t. t -> Definition t -> m (LayerData' t)
    -- cons7 :: forall t. t -> Definition t -> m (LayerData l t)
    -- cons7 :: forall t. a ~ Abstract t => t -> Definition t -> PMSubPass m (LayerData UID t)

newMagicElem :: forall t m. (IRMonad m, KnownType (Abstract t), IsIdx t) => Definition t -> m t
newMagicElem tdef = do
    irstate    <- getIR

    -- FIXME[WD]: how can we design it better?
    -- hacky, manual index reservation in order not to use keys for magic star
    let trep = typeVal' @(Abstract t)
        Just layerStore = irstate ^? wrapped'  . ix trep
    newIdx <- Store.reserveIdx layerStore


    let el = newIdx ^. from idx
    --     consLayer (layer, store) = runByIRBuilder $ do
    --         let consFunc = lookupLayerCons' (typeVal' @(Abstract t)) layer irstate
    --         Store.unsafeWrite store newIdx =<< unsafeAppCons consFunc el tdef
    -- mapM_ consLayer =<< Store.assocs layerStore
    return el
{-# INLINE newMagicElem #-}

type NewElemEvent m t = (Event.Emitter m (NEW // Abstract t), Event.Payload (NEW // Abstract t) ~ (Universal t, Prim.Any))
newElem :: forall t m. ( IRMonad m, Accessible (Net (Abstract t)) m, NewElemEvent m t, IsIdx t, KnownType (Abstract t))
        => Definition t -> m t
newElem tdef = do
    irstate    <- getIR
    newIdx     <- reserveNewElemIdx @t
    -- layerStore <- readComp @(Net (Abstract t))
    let el = newIdx ^. from idx
    emit (NEW // abstract el) (universal el, unsafeCoerce tdef :: Prim.Any)
    -- emit (NEW // abstract el) (universal el)
    --     consLayer (layer, store) = runByIRBuilder $ do
    --         let consFunc = lookupLayerCons' (typeVal' @(Abstract t)) layer irstate
    --         Store.unsafeWrite store newIdx =<< unsafeAppCons consFunc el tdef
    -- mapM_ consLayer =<< Store.assocs layerStore
    return el
{-# INLINE newElem #-}



delete :: forall t m. (IRMonad m, IsIdx t, Accessible (Net (Abstract t)) m) => t -> m ()
delete t = flip Store.freeIdx (t ^. idx) =<< readComp @(Net (Abstract t)) ; {-# INLINE delete #-}

reserveNewElemIdx :: forall t m. (IRMonad m, Accessible (Net (Abstract t)) m) => m Int
reserveNewElemIdx = Store.reserveIdx =<< readComp @(Net (Abstract t)) ; {-# INLINE reserveNewElemIdx #-}

readLayerByKey :: (IRMonad m, IsIdx t) => Key m (Layer (Abstract t) layer) -> t -> m (LayerData layer t)
readLayerByKey key t = unsafeCoerce <$> (Store.unsafeRead (t ^. idx) =<< readKey key) ; {-# INLINE readLayerByKey #-}

writeLayerByKey :: (IRMonad m, IsIdx t) => Key m (Layer (Abstract t) layer) -> LayerData layer t -> t -> m ()
writeLayerByKey key val t = (\v -> Store.unsafeWrite v (t ^. idx) $ unsafeCoerce val) =<< readKey key ; {-# INLINE writeLayerByKey #-}

readLayer :: forall layer t m. (IRMonad m, IsIdx t, Readable (Layer (Abstract t) layer) m) => t -> m (LayerData layer t)
readLayer t = flip readLayerByKey t =<< getKey @(Layer (Abstract t) layer) ; {-# INLINE readLayer #-}

writeLayer :: forall layer t m. (IRMonad m, IsIdx t, Readable (Layer (Abstract t) layer) m) => LayerData layer t -> t -> m ()
writeLayer val t = (\k -> writeLayerByKey k val t) =<< getKey @(Layer (Abstract t) layer) ; {-# INLINE writeLayer #-}

-- readAttr :: forall a m. (IRMonad m, Readable (Attr a) m) => m (KeyData m (Attr a))
-- readAttr = readComp @(Attr a) ; {-# INLINE readAttr #-}

readAttr :: forall a m. Readable (Attr a) m => m (KeyData m (Attr a))
readAttr = readComp @(Attr a) ; {-# INLINE readAttr #-}

writeAttr :: forall a m. Writable (Attr a) m => KeyData m (Attr a) -> m ()
writeAttr a = writeComp @(Attr a) a

readNet :: forall a m. (IRMonad m, Readable (Net a) m) => m (KeyData m (Net a))
readNet = readComp @(Net a) ; {-# INLINE readNet #-}


-- === Registration === --

registerElemWith :: forall el m. (KnownType el, IRMonad m) => (ElemStoreM m -> ElemStoreM m) -> m ()
registerElemWith = modifyIRM_ . modifyElem (typeVal' @el) ; {-# INLINE registerElemWith #-}

registerElem :: forall el m. (KnownType el, IRMonad m) => m ()
registerElem = registerElemWith @el id ; {-# INLINE registerElem #-}

-- registerGenericLayer :: forall layer t m. (IRMonad m, KnownType layer)
--                      => LayerCons' layer t m -> m ()
-- registerGenericLayer f = modifyIR_ $ genericLayers %~ Map.insert (typeVal' @layer) (anyCons @layer f)
-- {-# INLINE registerGenericLayer #-}
--
-- registerElemLayer :: forall at layer t m. (IRMonad m, KnownType at, KnownType layer)
--                   => LayerCons' layer t m -> m ()
-- registerElemLayer f = modifyIR_ $ specificLayers (typeVal' @at) %~ Map.insert (typeVal' @layer) (anyCons @layer f)
-- {-# INLINE registerElemLayer #-}

attachLayerIR :: IRMonad m => LayerRep -> ElemRep -> m ()
attachLayerIR l e = do
    s <- getIR
    let Just estore = s ^? wrapped' . ix e -- Internal error if not found (element not registered)
    Store.unsafeAddKey l estore
{-# INLINE attachLayerIR #-}

-- setAttr :: forall a m. (IRMonad m, KnownType a) => a -> m ()
-- setAttr a = modifyIR_ $ attrs %~ Map.insert (typeVal' @a) (unsafeCoerce a) ; {-# INLINE setAttr #-}




----------------------
-- === IRMonad === ---
----------------------

-- === Definition === --

-- | IRMonad is subclass of MonadFix because many expr operations reuire recursive calls.
--   It is more convenient to store it as global constraint, so it could be altered easily in the future.
type  IRMonadBase   m = (PrimMonad   m, MonadFix m)
type  IRMonadBaseIO m = (IRMonadBase m, MonadIO  m)
class IRMonadBase m => IRMonad m where
    getIR :: m (IRM m)
    putIR :: IRM m -> m ()

instance {-# OVERLAPPABLE #-} IRMonadBase m => IRMonad (IRBuilder m) where
    getIR = wrap'   State.get ; {-# INLINE getIR #-}
    putIR = wrap' . State.put ; {-# INLINE putIR #-}

instance {-# OVERLAPPABLE #-} IRMonadTrans t m => IRMonad (t m) where
    getIR = lift   getIR ; {-# INLINE getIR #-}
    putIR = lift . putIR ; {-# INLINE putIR #-}

type IRMonadTrans t m = (IRMonad m, MonadTrans t, IRMonadBase (t m), PrimState (t m) ~ PrimState m)


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

type instance KeyData m (Layer _ _) = LayerSet    (PrimState m)
type instance KeyData m (Net   _)   = ElemStoreST (PrimState m)
-- type instance KeyData _ (Attr  a)   = a


-- === Aliases === --

data Net  t
data Attr t

newtype AttrRep = AttrRep TypeRep deriving (Show, Eq, Ord)
instance IsTypeRep AttrRep
makeWrapped '' AttrRep

-- === Instances === --

instance (IRMonad m, KnownType e, KnownType l, EqPrimStates m n) => KeyMonad (Layer e l) m n where
    uncheckedLookupKey = do
        s <- getIR
        let mlv = s ^? wrapped' . ix (typeVal' @e)
        mr <- mapM (Store.readKey (typeVal' @l)) mlv
        return $ wrap' <$> join mr
    {-# INLINE uncheckedLookupKey #-}

instance (IRMonad m, KnownType a, EqPrimStates m n) => KeyMonad (Net a) m n where
    uncheckedLookupKey = fmap wrap' . (^? (wrapped' . ix (typeVal' @a))) <$> getIR ; {-# INLINE uncheckedLookupKey #-}

-- instance (IRMonad m, KnownType a) => KeyMonad (Attr a) m where
--     uncheckedLookupKey = fmap unsafeCoerce . (^? (attrs . ix (typeVal' @a))) <$> getIR ; {-# INLINE uncheckedLookupKey #-}


-------------------
-- === Link === --
-------------------

-- === Abstract === --

data LINK  a b
type LINK' a = LINK a a
type instance Definition (LINK a b) = (a,b)
type instance Abstract   (LINK a b) = LINK (Abstract  a) (Abstract  b)
type instance Universal  (LINK a b) = LINK (Universal a) (Universal b)


-- === Elem === --

type Link  a b = Elem (LINK a b)
type Link' a   = Link a a

type SubLink s t = Link (Sub s t) t



-- === Construction === --


magicLink :: forall a b m. (IRMonad m, KnownType (Abstract (Link a b)))
          => a -> b -> m (Link a b)
magicLink a b = newMagicElem (a,b) ; {-# INLINE magicLink #-}

link :: forall a b m. (IRMonad m, KnownType (Abstract (Link a b)), NewElemEvent m (Link a b), Accessible (Net (Abstract (Link a b))) m)
     => a -> b -> m (Link a b)
link a b = newElem (a,b) ; {-# INLINE link #-}





-------------------
-- === Group === --
-------------------

-- === Abstract === --

data GROUP a
type instance Definition (GROUP a) = Set a
type instance Abstract   (GROUP a) = GROUP (Abstract  a)
type instance Universal  (GROUP a) = GROUP (Universal a)

type Group a = Elem (GROUP a)


-- === Construction === --

group :: forall f a m. (IRMonad m, Foldable f, Ord a, NewElemEvent m (Group a), KnownType (Abstract (Group a)), Accessible (Net (Abstract (Group a))) m)
      => f a -> m (Group a)
group = newElem . foldl' (flip Set.insert) mempty ; {-# INLINE group #-}




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
type instance Access TERM    (ExprTerm atom t) = ExprTerm atom t

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

class                                                              TermEncoder atom where encodeTerm :: forall t. ExprTerm atom t -> ExprStore
instance                                                           TermEncoder I    where encodeTerm = impossible
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

magicExpr :: forall atom layout m. (TermEncoder atom, IRMonad m)
          => ExprTerm atom (Expr layout) -> m (Expr layout)
magicExpr a = newMagicElem (encodeTerm a) ; {-# INLINE magicExpr #-}

expr :: forall atom layout m. (TermEncoder atom, IRMonad m, NewElemEvent m (Expr layout), Accessible (Net EXPR) m)
     => ExprTerm atom (Expr layout) -> m (Expr layout)
expr = newElem . encodeTerm ; {-# INLINE expr #-}

-- class SomeGeneralEncode a where
--     someGeneralEncode :: a -> ExprStore
--
-- expr2 :: forall a layout m. (IRMonad m, Accessible ExprNet m, SomeGeneralEncode a)
--      => a -> m (Expr layout)
-- expr2 = newElem . someGeneralEncode ; {-# INLINE expr2 #-}

exprs :: (IRMonad m, Readable ExprNet m) => m [AnyExpr]
exprs = uncheckedElems ; {-# INLINE exprs #-}

links :: (IRMonad m, Readable ExprLinkNet m) => m [AnyExprLink]
links = uncheckedElems ; {-# INLINE links #-}


-- | Expr pattern matching utility
match :: (IRMonad m, Readable (Layer EXPR Model) m)
      => Expr layout -> (Unwrapped (ExprUniTerm (Expr layout)) -> m a) -> m a
match t f = f . unwrap' =<< (exprUniTerm t) ; {-# INLINE match #-}

-- | Term unification
exprUniTerm :: (IRMonad m, Readable (Layer EXPR Model) m) => Expr layout -> m (ExprUniTerm (Expr layout))
exprUniTerm t = ExprUniTerm <$> symbolMapM_AB @ToUniTerm toUniTerm t ; {-# INLINE exprUniTerm #-}

class ToUniTerm a b where toUniTerm :: a -> b
instance (Unwrapped a ~ Term t l, b ~ UniTerm l, IsUniTerm t l, Wrapped a)
      => ToUniTerm a b where toUniTerm = uniTerm . unwrap' ; {-# INLINE toUniTerm #-}


-- === Instances === --

type instance Event.Payload (NEW // EXPR)       = (AnyExpr, Prim.Any)
type instance Event.Payload (NEW // LINK' EXPR) = (Link' AnyExpr, Prim.Any) -- FIXME[WD]: refactor

type instance Sub s     (Expr l) = Expr (Sub s l)


type instance Generalizable (Expr l) (Expr l') = Generalizable l l'



-- -------------------------------------
-- === Expr Layout type caches === --
-------------------------------------

type instance Encode2 Atom    v = List.Index v (Every Atom)
type instance Encode2 Format  v = List.Index v (Every Format)




-- TO REFACTOR:

type instance UnsafeGeneralizable (Expr l) (Expr l') = ()

type family         UnsafeGeneralizable a b :: Constraint
unsafeGeneralize :: UnsafeGeneralizable a b => a -> b
unsafeGeneralize = unsafeCoerce ; {-# INLINE unsafeGeneralize #-}




type ExprLayer     = Layer EXPR
type ExprLinkLayer = Layer (LINK' EXPR)
type ExprNet       = Net   EXPR
type ExprLinkNet   = Net   (LINK' EXPR)
type ExprGroupNet  = Net   (GROUP EXPR)


type ExprLayers     ls = ExprLayer     <$> ls
type ExprLinkLayers ls = ExprLinkLayer <$> ls
type Nets           ls = Net           <$> ls

type Accessibles m lst = (Readables m lst, Writables m lst)

type family Readables m lst :: Constraint where
    Readables m '[]       = ()
    Readables m (l ': ls) = (Readable l m, Readables m ls)

type family Writables m lst :: Constraint where
    Writables m '[]       = ()
    Writables m (l ': ls) = (Writable l m, Writables m ls)



unsafeToExprTerm :: forall atom l m. (IRMonad m, Readable (ExprLayer Model) m) => Expr l -> m (ExprTerm atom (Expr l))
unsafeToExprTerm = unsafeCoerce . unwrap' . access @TERM . unwrap' <∘> readLayer @Model ; {-# INLINE unsafeToExprTerm #-}

unsafeToExprTermDef :: forall atom l m. (IRMonad m, Readable (ExprLayer Model) m) => Expr l -> m (ExprTermDef atom (Expr l))
unsafeToExprTermDef = unwrap' <∘> unsafeToExprTerm ; {-# INLINE unsafeToExprTermDef #-}







-- === Term mapping === --
-- | General expr symbol mapping utility. It allows mapping over current symbol in any expr.

class    IRMonad m => TermMapM (atoms :: [*]) ctx expr m b where symbolMapM :: (forall a. ctx a m b => a -> m b) -> expr -> m b
instance IRMonad m => TermMapM '[]            ctx expr m b where symbolMapM _ _ = impossible
instance (  TermMapM as ctx expr m b
         , ctx (ExprTerm a expr) m b
         , idx ~ FromJust (Encode2 Atom a) -- FIXME: make it nicer and assert
         , KnownNat idx
         , Readable (Layer EXPR Model) m
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
