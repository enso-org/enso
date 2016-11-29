{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE UndecidableInstances    #-}

module Luna.IR.Internal.IR where

import           Old.Data.Record              (Encode2)
import           Old.Data.Record.Model.Masked as X (TermRecord, VGRecord2, Store2(Store2), Slot(Slot), Enum(Enum))
import           Old.Data.Record.Model.Masked (encode2, EncodeStore, encodeStore, Mask, encodeNat, encodeData2, checkData2, decodeData2, Raw(Raw), unsafeRestore, decodeNat)

import           Luna.Prelude                 hiding (Symbol, typeRep, elem {- fix: -} , Enum)

import Control.Monad.State  (StateT, runStateT)
import Data.ManagedVectorMap(ManagedVectorMap, ManagedVectorMapM)
import Data.Map             (Map)
import Data.Property
import Data.RTuple          (TMap(..), empty, Assoc(..), Assocs, (:=:)) -- refactor empty to another library
import Data.Typeable        (Typeable, TypeRep)
import GHC.Prim             (Any)
import Luna.IR.Layer
import Luna.IR.Layer.Model
import Luna.IR.Term.Atom    (Atom, Atoms)
import Luna.IR.Term.Format  (Sub, Format, Draft)
import Luna.IR.Term.Layout  (Layout, LayoutOf, Name, Generalize, Universal, universal, Abstract)
import Luna.IR.Term.Symbol  (Sym, Symbol, UncheckedFromSymbol, FromSymbol, UniSymbol, IsUniSymbol, uniSymbol)
import Type.Container       (Every)
import Type.Container       (In)
import Type.Maybe           (FromJust)
import Type.Error
import Unsafe.Coerce        (unsafeCoerce)

import qualified Control.Monad.State       as State
import qualified Data.ManagedVectorMap     as MV
import qualified Data.Map                  as Map
import qualified Data.Set                  as Data (Set)
import qualified Data.Set                  as Set
import qualified Data.Typeable             as Typeable -- FIXME
import qualified Luna.IR.Term.Layout       as Layout
import qualified Luna.IR.Term.Symbol.Named as N
import qualified Type.List                 as List



typeRep :: forall a. Typeable a => TypeRep
typeRep = Typeable.typeRep (Proxy :: Proxy a) ; {-# INLINE typeRep #-}

type Typeables ts = Constraints $ Typeable <$> ts



class IsIdx t where
    idx :: Iso' t Int
    default idx :: (Wrapped t, Unwrapped t ~ Int) => Lens' t Int
    idx = wrapped' ; {-# INLINE idx #-}


------------------
-- === Elem === --
------------------

newtype Elem = Elem Int deriving (Show)
makeWrapped '' Elem

class IsElem a where
    elem :: Iso' a Elem
    default elem :: (Wrapped a, Unwrapped a ~ Elem) => Iso' a Elem
    elem = wrapped' ; {-# INLINE elem #-}

instance IsIdx Elem where
    idx = wrapped' ; {-# INLINE idx #-}



------------------
-- === Keys === --
------------------

--- === Definition === --

newtype Key k = Key (KeyData k)

newtype KeyDataST m key = KeyDataST (KeyTargetST m key)
data    KeyData     key where
        KeyData :: KeyDataST m key -> KeyData key

type family KeyTargetST (m :: * -> *) key


makeWrapped '' KeyDataST
makeWrapped '' Key


-- === Key Monad === --

class Monad m => KeyMonad key m where
    uncheckedLookupKeyDataST :: m (Maybe (KeyDataST m key))
--
uncheckedLookupKey :: KeyMonad key m => m (Maybe (Key key))
uncheckedLookupKey = (Key . keyData) .: uncheckedLookupKeyDataST ; {-# INLINE uncheckedLookupKey #-}


-- === Construction === --

keyData :: KeyDataST m k -> KeyData k
keyData = KeyData ; {-# INLINE keyData #-}

unsafeeFromKeyData :: KeyData k -> KeyDataST m k
unsafeeFromKeyData (KeyData d) = unsafeCoerce d ; {-# INLINE unsafeeFromKeyData #-}

unpackKey :: forall k m. Monad m => Key k -> m (KeyTargetST m k)
unpackKey k = return $ unwrap' (unsafeeFromKeyData $ unwrap' k :: KeyDataST m k) ; {-# INLINE unpackKey #-}

packKey :: forall k m. Monad m => KeyTargetST m k -> m (Key k)
packKey k = return . wrap' . keyData $ (wrap' k :: KeyDataST m k) ; {-# INLINE packKey #-}


-- === Key access === --

type Accessible k m = (Readable k m, Writable k m)

-- Readable
class    Monad m                                => Readable k m     where getKey :: m (Key k)
instance {-# OVERLAPPABLE #-} SubReadable k t m => Readable k (t m) where getKey = lift getKey ; {-# INLINE getKey #-}
type SubReadable k t m = (Readable k m, MonadTrans t, Monad (t m))

-- Writable
class    Monad m                                => Writable k m     where putKey :: Key k -> m ()
instance {-# OVERLAPPABLE #-} SubWritable k t m => Writable k (t m) where putKey = lift . putKey ; {-# INLINE putKey #-}
type SubWritable k t m = (Writable k m, MonadTrans t, Monad (t m))


read :: forall k m. Readable k m => m (KeyTargetST m k)
read = unpackKey =<< getKey @k ; {-# INLINE read #-}

write :: forall k m. Writable k m => KeyTargetST m k -> m ()
write = putKey @k <=< packKey ; {-# INLINE write #-}


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


data IRState   m = IRState   { _elems         :: Map ElemRep $ ElemStore m
                             , _attrs         :: Map LayerRep Any
                             , _genericLayers :: LayerConsStore m
                             }

data ElemStore m = ElemStore { _layerValues   :: ManagedVectorMapM m LayerRep Any
                             , _elemLayers    :: LayerConsStore m
                             }

type LayerConsStore m = Map LayerRep (AnyCons m)

makeLenses ''ElemStore
makeLenses ''IRState


-- === Accessors === --

specificLayers :: ElemRep -> Traversal' (IRState m) (LayerConsStore m)
specificLayers el = elems . ix el . elemLayers ; {-# INLINE specificLayers #-}


-- === Instances === --

instance Default (ElemStore m) where def = ElemStore def def
instance Default (IRState   m) where def = IRState   def def def



-----------------
-- === IRT === --
-----------------

-- === Definition === --

newtype IRT m a = IRT (StateT (IRState (IRT m)) m a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)
makeWrapped ''IRT

type IRState' m = IRState (GetIRMonad m)

type        GetIRMonad    m = IRT (GetIRSubMonad m)
type family GetIRSubMonad m where
            GetIRSubMonad (IRT m) = m
            GetIRSubMonad (t   m) = GetIRSubMonad m


-- === Accessors === --

atElem :: Functor m => ElemRep -> (Maybe (ElemStore m) -> m (Maybe (ElemStore m))) -> IRState m -> m (IRState m)
atElem = elems .: at  ; {-# INLINE atElem #-}

modifyElem  ::              ElemRep -> (ElemStore m ->    ElemStore m)  -> IRState m ->    IRState m
modifyElemM :: Functor m => ElemRep -> (ElemStore m -> m (ElemStore m)) -> IRState m -> m (IRState m)
modifyElem  e f = elems %~ Map.insertWith (const f) e (f def) ; {-# INLINE modifyElem  #-}
modifyElemM e f = atElem e $ fmap Just . f . fromMaybe def    ; {-# INLINE modifyElemM #-}


-- === Querying === --

lookupGenericLayerCons :: LayerRep -> IRState m -> Maybe (AnyCons m)
lookupGenericLayerCons l s = s ^? genericLayers . ix l ; {-# INLINE lookupGenericLayerCons #-}

lookupSpecificLayerCons :: ElemRep -> LayerRep -> IRState m -> Maybe (AnyCons m)
lookupSpecificLayerCons el l s = s ^? specificLayers el . ix l ; {-# INLINE lookupSpecificLayerCons #-}

lookupLayerCons :: ElemRep -> LayerRep -> IRState m -> Maybe (AnyCons m)
lookupLayerCons el l s = lookupSpecificLayerCons el l s <|> lookupGenericLayerCons l s ; {-# INLINE lookupLayerCons #-}

lookupLayerCons' :: ElemRep -> LayerRep -> IRState m -> AnyCons m
lookupLayerCons' = fromMaybe (error "Fatal error") .:. lookupLayerCons ; {-# INLINE lookupLayerCons' #-}


-- === Construction === --

newMagicElem :: forall t m. (IRMonad m, Typeable (Abstract t), PrimMonad (GetIRMonad m), IsElem t) => Definition t -> m t
newMagicElem tdef = do
    irstate    <- getIRState

    -- FIXME[WD]: how can we design it better?
    -- hacky, manual index reservation in order not to use keys for magic star
    let trep = typeRep @(Abstract t)
        Just layerStore = irstate ^? elems  . ix trep . layerValues
    (newIdx, layerStore') <- runInIR $ MV.reserveIdx layerStore
    putIRState $ irstate & elems . ix trep . layerValues .~ layerStore'


    let el = newIdx ^. from (elem . idx)
        consLayer (layer, store) = runInIR $ do
            let consFunc = lookupLayerCons' (typeRep @(Abstract t)) layer irstate
            MV.unsafeWrite store newIdx =<< unsafeAppCons consFunc el tdef
    mapM_ consLayer (MV.assocs layerStore)
    return el
{-# INLINE newMagicElem #-}

newElem :: forall t m. (IRMonad m, Accessible (Net (Abstract t)) m, IsElem t, Typeable (Abstract t)) => Definition t -> m t
newElem tdef = do
    irstate    <- getIRState
    newIdx     <- reserveNewElemIdx @t
    layerStore <- view layerValues <$> read @(Net (Abstract t))
    let el = newIdx ^. from (elem . idx)
        consLayer (layer, store) = runInIR $ do
            let consFunc = lookupLayerCons' (typeRep @(Abstract t)) layer irstate
            MV.unsafeWrite store newIdx =<< unsafeAppCons consFunc el tdef
    mapM_ consLayer (MV.assocs layerStore)
    return el
{-# INLINE newElem #-}


reserveNewElemIdx :: forall t m. (IRMonad m, Accessible (Net (Abstract t)) m) => m Int
reserveNewElemIdx = do
    elemStore <- read @(Net (Abstract t))
    (i, layerStore) <- runInIR $ MV.reserveIdx (elemStore ^. layerValues)
    write @(Net (Abstract t)) $ elemStore & layerValues .~ layerStore
    return i

unsafeReadLayerST :: (IRMonad m, IsElem t) => KeyDataST m (Layer (Abstract t) layer) -> t -> m (LayerData layer t)
unsafeReadLayerST key t = unsafeCoerce <$> runInIR (MV.unsafeRead (t ^. elem . idx) (unwrap' key)) ; {-# INLINE unsafeReadLayerST #-}

unsafeReadLayer :: (IRMonad m, IsElem t) => Key (Layer (Abstract t) layer) -> t -> m (LayerData layer t)
unsafeReadLayer k = unsafeReadLayerST (unsafeeFromKeyData $ unwrap' k) ; {-# INLINE unsafeReadLayer #-}

readLayer :: forall layer t m. (IRMonad m, IsElem t, Readable (Layer (Abstract t) layer) m ) => t -> m (LayerData layer t)
readLayer t = flip unsafeReadLayer t =<< getKey @(Layer (Abstract t) layer)

readAttr :: forall a m. (IRMonad m, Readable (Attr a) m) => m a
readAttr = unwrap' . unsafeeFromKeyData . unwrap' <$> getKey @(Attr a) ; {-# INLINE readAttr #-} -- FIXME[WD]: make it less hacky


-- === Registration === --

registerElemWith :: forall el m. (Typeable el, IRMonad m) => (ElemStore (GetIRMonad m) -> ElemStore (GetIRMonad m)) -> m ()
registerElemWith f = modifyIRState_ $ modifyElem (typeRep @el) f
{-# INLINE registerElemWith #-}

registerElem :: forall el m. (Typeable el, IRMonad m) => m ()
registerElem = registerElemWith @el id ; {-# INLINE registerElem #-}

registerGenericLayer :: forall layer t m. (IRMonad m, Typeable layer)
                     => LayerCons' layer t (GetIRMonad m) -> m ()
registerGenericLayer f = modifyIRState_ $ genericLayers %~ Map.insert (typeRep @layer) (anyCons @layer f)
{-# INLINE registerGenericLayer #-}

registerElemLayer :: forall at layer t m. (IRMonad m, Typeable at, Typeable layer)
                  => LayerCons' layer t (GetIRMonad m) -> m ()
registerElemLayer f = modifyIRState_ $ specificLayers (typeRep @at) %~ Map.insert (typeRep @layer) (anyCons @layer f)
{-# INLINE registerElemLayer #-}

attachLayer :: (IRMonad m, PrimMonad (GetIRMonad m)) => LayerRep -> ElemRep -> m ()
attachLayer l e = modifyIRStateM_ $ runInIR . modifyElemM e (layerValues $ MV.unsafeAddKey l)
{-# INLINE attachLayer #-}

setAttr :: forall a m. (IRMonad m, Typeable a) => a -> m ()
setAttr a = modifyIRState_ $ attrs %~ Map.insert (typeRep @a) (unsafeCoerce a) ; {-# INLINE setAttr #-}

----------------------
-- === IRMonad === ---
----------------------

-- === Definition === --

-- | IRMonad is subclass of MonadFic because many term operations reuire recursive calls.
--   It is more convenient to store it as global constraint, so it could be altered easily in the future.
type  IRMonadBase       m = (PrimMonad m, MonadFix m)
type  IRMonadInvariants m = (IRMonadBase m, IRMonadBase (GetIRSubMonad m), IRMonad (GetIRMonad m))
class IRMonadInvariants m => IRMonad m where
    getIRState :: m (IRState' m)
    putIRState :: IRState' m -> m ()
    runInIR    :: GetIRMonad m a -> m a

instance {-# OVERLAPPABLE #-} (MonadFix m, PrimMonad m) => IRMonad (IRT m) where
    getIRState = wrap'   State.get ; {-# INLINE getIRState #-}
    putIRState = wrap' . State.put ; {-# INLINE putIRState #-}
    runInIR    = id                ; {-# INLINE runInIR    #-}

instance {-# OVERLAPPABLE #-} IRMonadTrans t m => IRMonad (t m) where
    getIRState = lift   getIRState ; {-# INLINE getIRState #-}
    putIRState = lift . putIRState ; {-# INLINE putIRState #-}
    runInIR    = lift . runInIR    ; {-# INLINE runInIR    #-}

type IRMonadTrans t m = (IRMonad m, MonadTrans t, IRMonadBase (t m), GetIRMonad (t m) ~ GetIRMonad m)


-- === Modyfication === --

modifyIRStateM :: IRMonad m => (IRState' m -> m (a, IRState' m)) -> m a
modifyIRStateM f = do
    s <- getIRState
    (a, s') <- f s
    putIRState s'
    return a
{-# INLINE modifyIRStateM #-}

modifyIRStateM_ :: IRMonad m => (IRState' m -> m (IRState' m)) -> m ()
modifyIRStateM_ = modifyIRStateM . fmap (fmap ((),)) ; {-# INLINE modifyIRStateM_ #-}

modifyIRState_ :: IRMonad m => (IRState' m -> IRState' m) -> m ()
modifyIRState_ = modifyIRStateM_ . fmap return ; {-# INLINE modifyIRState_ #-}


-- === Running === --

runIRT :: forall t m a. Monad m => IRT m a -> m a
runIRT m = State.evalStateT (unwrap' m) def ; {-# INLINE runIRT #-}


-- === Instances === --

instance MonadTrans IRT where
    lift = wrap' . lift ; {-# INLINE lift #-}

instance PrimMonad m => PrimMonad (IRT m) where
    type PrimState (IRT m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}


-----------------------
-- === Key types === --
-----------------------

-- === Definitions === --

type instance KeyTargetST m (Layer _ _) = MV.VectorRefM (GetIRMonad m) Any -- FIXME: make the type nicer
type instance KeyTargetST m (Net   _)   = ElemStore     (GetIRMonad m)
type instance KeyTargetST m (Attr  a)   = a


-- === Aliases === --

data Net  t
data Attr t

type LayerKey el l = Key (Layer el l)
type NetKey  n     = Key (Net   n)
type AttrKey  a    = Key (Attr  a)


-- === Instances === --

instance (IRMonad m, Typeable e, Typeable l) => KeyMonad (Layer e l) m where
    uncheckedLookupKeyDataST = fmap wrap' . (^? (elems . ix (typeRep @e) . layerValues . ix (typeRep @l))) <$> getIRState ; {-# INLINE uncheckedLookupKeyDataST #-}

instance (IRMonad m, Typeable a) => KeyMonad (Net a) m where
    uncheckedLookupKeyDataST = fmap wrap' . (^? (elems . ix (typeRep @a))) <$> getIRState ; {-# INLINE uncheckedLookupKeyDataST #-}

instance (IRMonad m, Typeable a) => KeyMonad (Attr a) m where
    uncheckedLookupKeyDataST = fmap unsafeCoerce . (^? (attrs . ix (typeRep @a))) <$> getIRState ; {-# INLINE uncheckedLookupKeyDataST #-}





-------------------
-- === Link === --
-------------------

-- === Definition === --

newtype Link  a b = Link Elem deriving (Show)
type    Link' a   = Link a a
type instance Definition (Link a b) = (a,b)
makeWrapped ''Link

type SubLink s t = Link (Sub s t) t

-- === Abstract === --

data LINK  a b
type LINK' a = LINK a a
type instance Abstract  (Link a b) = LINK (Abstract  a) (Abstract  b)


-- === Construction === --

magicLink :: forall a b m. (IRMonad m, Typeable (Abstract a), Typeable (Abstract b))
          => a -> b -> m (Link a b)
magicLink a b = newMagicElem (a,b) ; {-# INLINE magicLink #-}

link :: forall a b m. (IRMonad m, Typeable (Abstract a), Typeable (Abstract b), Accessible (Net (Abstract (Link a b))) m)
     => a -> b -> m (Link a b)
link a b = newElem (a,b) ; {-# INLINE link #-}


-- === Instances === --

instance      IsElem    (Link a b)
type instance Universal (Link a b) = Link (Universal a) (Universal b)




------------------------
-- === TermSymbol === --
------------------------

data XXX -- FIXME

newtype TermSymbol    atom t = TermSymbol    (N.Symbol atom (Layout.Named (SubLink Name t) (SubLink Atom t)))
newtype TermUniSymbol      t = TermUniSymbol (N.UniSymbol   (Layout.Named (SubLink Name t) (SubLink Atom t)))
type    TermSymbol'   atom   = TermSymbol atom XXX
makeWrapped ''TermSymbol
makeWrapped ''TermUniSymbol


-- === Helpers === --

hideLayout :: TermSymbol atom t -> TermSymbol atom XXX
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
type ValidateLayout' t     sel a = ValidateLayout (t # Layout) sel a


-- === Instances === --

-- FIXME: [WD]: it seems that Layout in the below declaration is something else than real layout - check it and refactor
type instance Access Layout (TermSymbol atom t) = Access Layout (Unwrapped (TermSymbol atom t))
type instance Access Atom   (TermSymbol atom t) = atom
type instance Access Format (TermSymbol atom t) = Access Format atom
type instance Access Sym    (TermSymbol atom t) = TermSymbol atom t

instance Accessor Sym (TermSymbol atom t) where access = id ; {-# INLINE access #-}

instance UncheckedFromSymbol (TermSymbol atom t) where uncheckedFromSymbol = wrap' ; {-# INLINE uncheckedFromSymbol #-}

instance ValidateLayout (LayoutOf t) Atom atom
      => FromSymbol (TermSymbol atom t) where fromSymbol = wrap' ; {-# INLINE fromSymbol #-}


-- Repr
instance Repr s (Unwrapped (TermSymbol atom t))
      => Repr s (TermSymbol atom t) where repr = repr . unwrap' ; {-# INLINE repr #-}

-- Fields
type instance FieldsType (TermSymbol atom t) = FieldsType (Unwrapped (TermSymbol atom t))
instance HasFields (Unwrapped (TermSymbol atom t))
      => HasFields (TermSymbol atom t) where fieldList = fieldList . unwrap' ; {-# INLINE fieldList #-}



----------------------
-- === TermData === --
----------------------

type TermStoreSlots = '[ Atom ':= Enum, Format ':= Mask, Sym ':= Raw ]
type TermStore = Store2 TermStoreSlots

newtype TermData sys model = TermData TermStore deriving (Show)
makeWrapped ''TermData


-- === Encoding === --

class                                                              SymbolEncoder atom where encodeSymbol :: forall t. TermSymbol atom t -> TermStore
instance                                                           SymbolEncoder I    where encodeSymbol = impossible
instance EncodeStore TermStoreSlots (TermSymbol' atom) Identity => SymbolEncoder atom where
    encodeSymbol = runIdentity . encodeStore . hideLayout ; {-# INLINE encodeSymbol #-} -- magic



------------------
-- === Term === --
------------------

-- === Definition === --

newtype Term  layout = Term Elem deriving (Show)
type    Term'        = Term Draft
makeWrapped ''Term

type instance Definition (Term _) = TermStore


-- === Abstract === --

data TERM
type instance Abstract (Term _) = TERM


-- === Utils === --

magicTerm :: forall atom layout m. (SymbolEncoder atom, IRMonad m)
          => TermSymbol atom (Term layout) -> m (Term layout)
magicTerm a = newMagicElem (encodeSymbol a) ; {-# INLINE magicTerm #-}

term :: forall atom layout m. (SymbolEncoder atom, IRMonad m, Accessible (Net TERM) m)
     => TermSymbol atom (Term layout) -> m (Term layout)
term a = newElem (encodeSymbol a) ; {-# INLINE term #-}

-- | Term pattern matching utility
match :: (IRMonad m, Readable (Layer TERM Model) m)
      => Term layout -> (Unwrapped (TermUniSymbol (Term layout)) -> m a) -> m a
match t f = f . unwrap' =<< (exprUniSymbol t) ; {-# INLINE match #-}

-- | Symbol unification
exprUniSymbol :: (IRMonad m, Readable (Layer TERM Model) m) => Term layout -> m (TermUniSymbol (Term layout))
exprUniSymbol t = TermUniSymbol <$> symbolMapM_AB @ToUniSymbol toUniSymbol t ; {-# INLINE exprUniSymbol #-}

class ToUniSymbol a b where toUniSymbol :: a -> b
instance (Unwrapped a ~ Symbol t l, b ~ UniSymbol l, IsUniSymbol t l, Wrapped a)
      => ToUniSymbol a b where toUniSymbol = uniSymbol . unwrap' ; {-# INLINE toUniSymbol #-}


-- === Instances === --

instance      IsElem    (Term l)
type instance Universal (Term _) = Term'
type instance Sub s     (Term l) = Term (Sub s l)


-- === Symbol mapping === --
-- | General term symbol mapping utility. It allows mapping over current symbol in any term.

class    IRMonad m => SymbolMapM (atoms :: [*]) ctx expr m b where symbolMapM :: (forall a. ctx a m b => a -> m b) -> expr -> m b
instance IRMonad m => SymbolMapM '[]            ctx term m b where symbolMapM _ _ = impossible
instance (  SymbolMapM as ctx term m b
         , ctx (TermSymbol a term) m b
         , idx ~ FromJust (Encode2 Atom a) -- FIXME: make it nicer and assert
         , KnownNat idx
         , Readable (Layer TERM Model) m
         , term ~ Term layout
         )
      => SymbolMapM (a ': as) ctx term m b where
    symbolMapM f term = do
        d <- unwrap' <$> readLayer @Model term
        let eidx = unwrap' $ access @Atom d
            idx  = fromIntegral $ natVal (Proxy :: Proxy idx)
            sym  = unsafeCoerce (unwrap' $ access @Sym d) :: TermSymbol a (Term layout)
        if (idx == eidx) then f sym else symbolMapM @as @ctx f term
    {-# INLINE symbolMapM #-}


type SymbolMapM_AMB          = SymbolMapM     (Every Atom)
type SymbolMapM_AB  ctx      = SymbolMapM_AMB (DropMonad ctx)
type SymbolMap_AB   ctx expr = SymbolMapM_AB  ctx expr Identity
type SymbolMapM_A   ctx      = SymbolMapM_AB  (FreeResult ctx)
type SymbolMap_A    ctx expr = SymbolMapM_A   ctx expr Identity

symbolMapM_AMB :: forall ctx m expr b. SymbolMapM_AMB ctx expr m b => (forall a. ctx a m b => a -> m b) -> expr -> m b
symbolMapM_AB  :: forall ctx expr m b. SymbolMapM_AB  ctx expr m b => (forall a. ctx a   b => a ->   b) -> expr -> m b
symbolMap_AB   :: forall ctx expr   b. SymbolMap_AB   ctx expr   b => (forall a. ctx a   b => a ->   b) -> expr ->   b
symbolMapM_A   :: forall ctx expr m b. SymbolMapM_A   ctx expr m b => (forall a. ctx a     => a ->   b) -> expr -> m b
symbolMap_A    :: forall ctx expr   b. SymbolMap_A    ctx expr   b => (forall a. ctx a     => a ->   b) -> expr ->   b
symbolMapM_AMB   = symbolMapM @(Every Atom) @ctx                  ; {-# INLINE symbolMapM_AMB #-}
symbolMapM_AB  f = symbolMapM_AMB @(DropMonad ctx) (return <$> f) ; {-# INLINE symbolMapM_AB  #-}
symbolMap_AB   f = runIdentity . symbolMapM_AB @ctx f             ; {-# INLINE symbolMap_AB   #-}
symbolMapM_A     = symbolMapM_AB @(FreeResult ctx)                ; {-# INLINE symbolMapM_A   #-}
symbolMap_A    f = runIdentity . symbolMapM_A @ctx f              ; {-# INLINE symbolMap_A    #-}














class    (ctx a b, Monad m) => DropMonad ctx a m b
instance (ctx a b, Monad m) => DropMonad ctx a m b

class    ctx a => FreeResult ctx a b
instance ctx a => FreeResult ctx a b





-- -------------------------------------
-- === Expr Layout type caches === --
-------------------------------------

type instance Encode2 Atom    v = List.Index v (Every Atom)
type instance Encode2 Format  v = List.Index v (Every Format)



--
-- --------------------
-- -- === Events === --
-- --------------------
--
-- -- === Definition === --
--
-- type Register        t a m = Event        (t (Universal a)) m (Universal a)
-- type DelayedRegister t a m = DelayedEvent (t (Universal a)) m (Universal a)
--
--
-- -- === Registration === --
--
-- register :: forall t a m. Register t a m => a -> m a
-- register a = a <$ dispatch_ @(t (Universal a)) (universal a) ; {-# INLINE register #-}
--
-- delayedRegister :: forall t a m. DelayedRegister t a m => a -> m a
-- delayedRegister a = a <$ delayedDispatch_ @(t (Universal a)) (universal a) ; {-# INLINE delayedRegister #-}
--
--
-- -- === Event types === --
--
-- data New a
