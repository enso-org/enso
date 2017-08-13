{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# EXT      inlineAll               #-}


module OCI.IR.Class (module OCI.IR.Class, module X) where

import           Data.Record as X (Store(Store), Enum(Enum))
import           Data.Record

import           Luna.Prelude                 hiding (elem {- fix: -} , Enum, log)
import qualified Luna.Prelude as Prelude

import Control.Monad.State.Dependent -- (StateT, runStateT)
import Data.Map             (Map)
import Data.Property
import Data.RTuple          (TMap(..), empty, Assoc(..), Assocs, (:=:)) -- refactor empty to another library
import Prologue.Prim (AnyData)
import OCI.IR.Layer.Class (Definition, LayerRep, Layer, LayerData)
import OCI.IR.Layer.Model (Model)
import OCI.IR.Term    (TermType, TermTypesOf, TermDesc, atomDescOf, TermTypeOf)
import qualified OCI.IR.Term as A
import OCI.IR.Layout.Format    (Format)
import OCI.IR.Layout.Class   (LAYOUT, LayoutOf, Generalizable, Universal, universal, Abstract, Sub, abstract)
import OCI.IR.Term     (TERM, Term, UncheckedFromTerm, FromTerm)
-- import Luna.IR.Term.Uni (UniTerm, IsUniTerm, uniTerm)
import Type.Bool             (And)
import Type.Container        (Every)
import Type.Container        (In)
import Type.Maybe            (FromJust)
import Type.Error_old
import Unsafe.Coerce        (unsafeCoerce)

import qualified Control.Monad.State       as State
import qualified Data.ManagedVectorMap     as Store
import qualified Data.Map                  as Map
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import qualified OCI.IR.Layout.Class       as Layout
import qualified OCI.IR.Term   as N
import qualified Type.List                 as List
import qualified Data.Event                as Event
import           Data.Event                (Payload(Payload), Emitter, IsPayload, PayloadData, emit, type (//))
-- import Luna.IR.Term.Uni ()
import Data.TypeDesc
import Type.Bool (And)

import System.Log (MonadLogging, Logging, withDebugBy)
import           Control.Monad.Trans.Maybe (MaybeT)
import Data.Graph.Class as X (Net, TypeRepGraphST, TypeRepGraph, GetRefHandler, Elem(Elem), TypeRepGraphM, MonadRefState, TypeRepVectorMapST, RefData, ElemRep, TypeRepVectorMapM, GraphElem, RefData', Ref(Ref), Refs, MonadRefLookup, uncheckedLookupRef, MonadRefStore, uncheckedStoreRef, putRef, idx, getRefData, getRef, putRefData)
import Data.Container.Mutable as X (freeze, thaw, unsafeFreeze, unsafeThaw)
import Control.Monad.Raise
import Data.Container.Mutable
import qualified Data.Foldable as Foldable

type EqPrimStates m n = (PrimState m ~ PrimState n)





data Attr  = Attr  deriving (Show)

-- data Net  t
-- data Attr t

--------------------
-- === Events === --
--------------------

-- FIXME[WD]: rename events to: OnCreate, OnDelete, OnImport etc, because now they collide with too many things
data New             = New             deriving (Show)
data Delete          = Delete          deriving (Show)
data Import          = Import          deriving (Show)
data OnDeepDelete    = OnDeepDelete    deriving (Show)
data OnQueryChildren = OnQueryChildren deriving (Show)



------------------
-- === Elem === --
------------------


-- type family FromElem t where FromElem (Elem t) = t

type instance Definition (Elem t) = Definition t
type instance Abstract   (Elem t) = Elem (Abstract  t)
type instance Universal  (Elem t) = Elem (Universal t)




--------------------------------------
-- === Reader / Writer / Editor === --
--------------------------------------

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



----------------
-- === IR === --
----------------

-- === Definitions === --

newtype IR     = IR   TypeRepGraph       deriving (Show, Default)
newtype IRST s = IRST (TypeRepGraphST s) deriving (Default)
type    IRM  m = IRST (PrimState m)

makeWrapped ''IR
makeWrapped ''IRST


-- === HasIR === --

class    HasIR a  where ir :: Lens' a IR
instance HasIR IR where ir = id


-- === Instances === --

instance Show (IRST s) where show _ = "IRST"

type instance Index   IR = Index   (Unwrapped IR)
type instance IxValue IR = IxValue (Unwrapped IR)
instance      Ixed    IR where ix i = wrapped . ix i
instance      At      IR where at i = wrapped . at i

type instance Index   (IRST s) = Index   (Unwrapped (IRST s))
type instance IxValue (IRST s) = IxValue (Unwrapped (IRST s))
instance      Ixed    (IRST s) where ix i = wrapped . ix i
instance      At      (IRST s) where at i = wrapped . at i

-- Mutability
type instance Mutable s IR = IRST s
instance PrimMonad m => Freezable       m IR where freeze       = fmap wrap . freeze       . unwrap
instance PrimMonad m => UnsafeFreezable m IR where unsafeFreeze = fmap wrap . unsafeFreeze . unwrap
instance PrimMonad m => Thawable        m IR where thaw         = fmap wrap . thaw         . unwrap
instance PrimMonad m => UnsafeThawable  m IR where unsafeThaw   = fmap wrap . unsafeThaw   . unwrap


------------------------------------------
-- === Errors === --

data IRError = ElemLookupError ElemRep deriving (Show)
instance Exception IRError



-----------------------
-- === Rooted IR === --
-----------------------

-- === Definition === --

data Rooted     a = Rooted   { __ir :: !IR      , __elem :: !a } deriving (Show, Functor, Traversable, Foldable)
data RootedST s a = RootedST { __ir :: !(IRST s), __elem :: !a } deriving (Show, Functor, Traversable, Foldable)
type RootedM  m   = RootedST (PrimState m)
makeLenses ''Rooted
makeLenses ''RootedST


-- === Utils === --

rootedST :: MonadIR m => a -> m (RootedM m a)
rootedST a = RootedST <$> get @IRST <*> pure a

rooted :: MonadIR m => a -> m (Rooted a)
rooted = join . fmap freeze . rootedST


-- === HasRoot === --

class    HasRoot t where root :: Lens (t a) (t b) a b
instance HasRoot  Rooted      where root = rooted_elem
instance HasRoot (RootedST s) where root = rootedST_elem


-- === Instances ==== --

-- Accessors
instance HasIR     (Rooted a)   where ir      = rooted_ir
instance Copointed  Rooted      where copoint = view root
instance Copointed (RootedST s) where copoint = view root

-- Mutability
type instance Mutable s (Rooted a) = (RootedST s a)
instance PrimMonad m => Freezable       m (Rooted a) where freeze       ir = Rooted   <$> freeze       (ir ^. rootedST_ir) <*> pure (ir ^. root)
instance PrimMonad m => UnsafeFreezable m (Rooted a) where unsafeFreeze ir = Rooted   <$> unsafeFreeze (ir ^. rootedST_ir) <*> pure (ir ^. root)
instance PrimMonad m => Thawable        m (Rooted a) where thaw         ir = RootedST <$> thaw         (ir ^. rooted_ir)   <*> pure (ir ^. root)
instance PrimMonad m => UnsafeThawable  m (Rooted a) where unsafeThaw   ir = RootedST <$> unsafeThaw   (ir ^. rooted_ir)   <*> pure (ir ^. root)



-----------------------
-- === IRBuilder === --
-----------------------

-- === Definition === --

type IRBuilder m = StateT (IRM m) m -- FIXME[WD]: rename or remove


-- === Accessors === --

modifyElem  :: PrimMonad m => ElemRep -> (TypeRepVectorMapM m ->    TypeRepVectorMapM m)  -> IRM m -> m (IRM m)
modifyElemM :: PrimMonad m => ElemRep -> (TypeRepVectorMapM m -> m (TypeRepVectorMapM m)) -> IRM m -> m (IRM m)
modifyElem  e   = modifyElemM e . fmap return
modifyElemM e f = at e $ \es -> fmap Just $ f =<< fromMaybe (Store.empty) (fmap return es)


-- | The type `t` is not validated in any way, it is just constructed from index.
uncheckedElems :: forall t m. (MonadRef m, GraphElem t, Reader Net (Abstract t) m) => m [t]
uncheckedElems = fmap (view $ from idx) <$> (liftRefHandler . Store.ixes =<< readNet @(Abstract t))


-- === Construction === --


type NewElemEvent m t = (Emitter (New // Abstract t) m, Logging m, KnownType (Abstract t), GraphElem t)
newElem :: forall t m. ( MonadRef m, Writer Net (Abstract t) m, NewElemEvent m t)
        => Definition t -> m t
newElem tdef = do
    t <- reserveElem
    t <$ dispatchNewElem t tdef


instance IsPayload   (New // t) where
    type PayloadData (New // t) = (t, Definition t)

instance IsPayload   (Delete // t) where
    type PayloadData (Delete // t) = t

type instance Abstract (a // b) = Abstract a // Abstract b
type instance Abstract New             = New
type instance Abstract Delete          = Delete
type instance Abstract OnDeepDelete    = OnDeepDelete
type instance Abstract OnQueryChildren = OnQueryChildren
type instance Abstract Import          = Import

reserveElem :: forall t m. (MonadRef m, Writer Net (Abstract t) m, GraphElem t) => m t
reserveElem = view (from idx) <$> reserveNewElemIdx @t

dispatchNewElem :: forall t m. NewElemEvent m t => t -> Definition t -> m ()
dispatchNewElem t tdef = withDebugBy "Emitter" (convert $ "Event New // " <> show (getTypeDesc_ @(Abstract t)) <> " [" <> show (t ^. idx) <> "]")
                       $ emit $ Payload @(New // t) (t, tdef)


freeElem :: forall t m. (MonadRef m, GraphElem t, Writer Net (Abstract t) m) => t -> m ()
freeElem t = liftRefHandler . flip Store.freeIdx (t ^. idx) =<< getRefData @Net @(Abstract t)

delete :: forall t m. (MonadRef m, GraphElem t, Editor Net (Abstract t) m, Event.Emitter (Delete // Abstract t) m)
       => t -> m ()
delete t = emit (Payload @(Delete // t) t) >> freeElem t

reserveNewElemIdx :: forall t m. (MonadRef m, Writer Net (Abstract t) m) => m Int
reserveNewElemIdx = liftRefHandler . Store.reserveIdx =<< getRefData @Net @(Abstract t)

getLayerByRef :: (MonadIR m, GraphElem t) => Ref Layer (Abstract t // layer) m -> t -> m (LayerData layer t)
getLayerByRef key t = unsafeCoerce <$> (Store.unsafeRead (t ^. idx) $ unwrap' key)

putLayerByRef :: (MonadIR m, GraphElem t) => t -> Ref Layer (Abstract t // layer) m -> LayerData layer t -> m ()
putLayerByRef t key val = (\v -> Store.unsafeWrite v (t ^. idx) $ unsafeCoerce val) $ unwrap' key



-- === Layer management === --

type LayerReadCtx  s t m = (MonadRef m, GraphElem t, Reader Layer (Abstract t // s) m)
type LayerWriteCtx s t m = (MonadRef m, GraphElem t, Writer Layer (Abstract t // s) m)
type LayerEditCtx  s t m = (MonadRef m, GraphElem t, Editor Layer (Abstract t // s) m)

getLayer :: forall s t m. LayerReadCtx  s t m => t -> m (LayerData s t)
putLayer :: forall s t m. LayerWriteCtx s t m => t -> LayerData s t -> m ()
getLayer t     = liftRefHandler . flip getLayerByRef t          =<< getRef @Layer @(Abstract t // s)
putLayer t val = (\k -> liftRefHandler $ putLayerByRef t k val) =<< getRef @Layer @(Abstract t // s)

modifyLayerM  :: forall s t m a. LayerEditCtx s t m => t -> (LayerData s t -> m (a, LayerData s t)) -> m a
modifyLayerM_ :: forall s t m.   LayerEditCtx s t m => t -> (LayerData s t -> m    (LayerData s t)) -> m ()
modifyLayer   :: forall s t m a. LayerEditCtx s t m => t -> (LayerData s t ->   (a, LayerData s t)) -> m a
modifyLayer_  :: forall s t m.   LayerEditCtx s t m => t -> (LayerData s t ->       LayerData s t)  -> m ()
modifyLayer   t   = modifyLayerM  @s t . fmap return
modifyLayer_  t   = modifyLayerM_ @s t . fmap return
modifyLayerM_ t   = modifyLayerM  @s t . (fmap.fmap) ((),)
modifyLayerM  t f = do (a,s) <- f =<< getLayer @s t
                       a <$ putLayer @s t s

branchedLayer      :: forall s t m a. LayerEditCtx s t m => t ->                                         m a -> m a
withLayer          :: forall s t m a. LayerEditCtx s t m => t ->  LayerData s t                       -> m a -> m a
withModifiedLayer  :: forall s t m a. LayerEditCtx s t m => t -> (LayerData s t ->    LayerData s t)  -> m a -> m a
withModifiedMLayer :: forall s t m a. LayerEditCtx s t m => t -> (LayerData s t -> m (LayerData s t)) -> m a -> m a
withLayer          t     = withModifiedLayer  @s t . const
withModifiedLayer  t     = withModifiedMLayer @s t . fmap return
withModifiedMLayer t f m = branchedLayer      @s t $ modifyLayerM_ @s t f >> m
branchedLayer      t   m = do s <- getLayer @s t
                              m <* putLayer @s t s


-- === Attr management === --

getAttr :: forall a m. Reader Attr a m => m a
putAttr :: forall a m. Writer Attr a m => a -> m ()
getAttr = getRefData @Attr @a
putAttr = putRefData @Attr @a

modifyAttrM  :: forall s m a. Editor Attr s m => (s -> m (a, s)) -> m a
modifyAttrM_ :: forall s m.   Editor Attr s m => (s -> m     s)  -> m ()
modifyAttr   :: forall s m a. Editor Attr s m => (s ->   (a, s)) -> m a
modifyAttr_  :: forall s m.   Editor Attr s m => (s ->       s)  -> m ()
modifyAttr    = modifyAttrM  . fmap return
modifyAttr_   = modifyAttrM_ . fmap return
modifyAttrM_  = modifyAttrM  . (fmap.fmap) ((),)
modifyAttrM f = do (a,t) <- f =<< getAttr
                   a <$ putAttr t

branchedAttr      :: forall s m a. Editor Attr s m =>               m a -> m a
withAttr          :: forall s m a. Editor Attr s m => s          -> m a -> m a
withModifiedAttr  :: forall s m a. Editor Attr s m => (s ->   s) -> m a -> m a
withModifiedMAttr :: forall s m a. Editor Attr s m => (s -> m s) -> m a -> m a
withAttr              = withModifiedAttr  . const
withModifiedAttr      = withModifiedMAttr . fmap return
withModifiedMAttr f m = branchedAttr @s $ modifyAttrM_ f >> m
branchedAttr        m = do s <- getAttr @s
                           m <* putAttr @s s



readNet :: forall a m. Reader Net a m => m (RefData' Net a m)
readNet = getRefData @Net @a


-- === Registration === --

registerElemWith :: forall el m. (KnownType el, MonadIR m) => (TypeRepVectorMapM m -> TypeRepVectorMapM m) -> m ()
registerElemWith = modifyM_ @IRST . modifyElem (getTypeDesc @el)

registerElem :: forall el m. (KnownType el, MonadIR m) => m ()
registerElem = registerElemWith @el id

unsafeCreateNewLayer :: (MonadIR m, Throws IRError m) => LayerRep -> ElemRep -> m ()
unsafeCreateNewLayer l e = do
    s      <- get @IRST
    estore <- tryJust (ElemLookupError e) $ s ^? ix e
    Store.unsafeAddKey l estore




----------------------
-- === MonadIR === ---
----------------------

-- === Definition === --

-- | MonadIR is subclass of MonadFix because many expr operations reuire recursive calls.
--   It is more convenient to store it as global constraint, so it could be altered easily in the future.
type MonadIRBase   m = (PrimMonad   m, MonadFix m, Logging m, MonadIO m) -- FIXME[WD]: remove io
type MonadIRBaseIO m = (MonadIRBase m, MonadIO  m)
type MonadIR       m = (MonadIRBase m, MonadState (IRM m) m)

type MonadIRTrans t m = (MonadIR m, MonadTrans t, MonadIRBase (t m), PrimState (t m) ~ PrimState m)

instance s ~ IRM m => InferState IRST m s
-- instance {-# OVERLAPPABLE #-} s ~ IRM m => InferState IRST (t m) s
-- instance {-# OVERLAPPABLE #-} s ~ IRM m => InferState IRST (StateT x m) s


--FIXME: REFACTOR
class (MonadIR m, MonadIR (GetRefHandler m), EqPrims m (GetRefHandler m), GetRefHandler (GetRefHandler m) ~ GetRefHandler m)
   => MonadRef m where liftRefHandler :: forall a. GetRefHandler m a -> m a

-- instance {-# OVERLAPPABLE #-} (MonadTransInvariants' t m, GetRefHandler (t m) ~ GetRefHandler m, RefHandlerCtx (t m), MonadRef m)
instance {-# OVERLAPPABLE #-} (MonadTransInvariants' t m, GetRefHandler (t m) ~ GetRefHandler m, MonadRef m, MonadIR (t m))
      => MonadRef (t m) where
    liftRefHandler = lift . liftRefHandler


-- === Modyfication === --

snapshot :: MonadIR m => m IR
snapshot = freeze =<< get @IRST

subIR :: MonadIR m => m a -> m (IR, a)
subIR m = with @IRST def $ flip (,) <$> m <*> snapshot

runIR :: MonadIR m => m a -> m (IR, a)
runIR m = flip (,) <$> m <*> snapshot

withIR :: MonadIR m => IR -> m a -> m a
withIR ir m = do irst <- thaw ir
                 with @IRST irst m


-- === Running === --

evalIRBuilderM :: Monad m => StateT (IRM m) m a -> IRM m -> m a
evalIRBuilderM = evalStateT

evalIRBuilder :: PrimMonad m => StateT (IRM m) m a -> IR -> m a
evalIRBuilder m = evalIRBuilderM m . wrap <=< thaw . unwrap

evalIRBuilder' :: Monad m => StateT (IRM m) m a -> m a
evalIRBuilder' = flip evalIRBuilderM def



-----------------------
-- === Ref types === --
-----------------------

-- === Definitions === --

type LayerSet s = Store.VectorRef s AnyData

type instance RefData Layer _ m = LayerSet           (PrimState m)
type instance RefData Net   _ m = TypeRepVectorMapST (PrimState m)
type instance RefData Attr  a _ = a


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
link a b = newElem (a,b)


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
group = newElem . foldl' (flip Set.insert) mempty




---------------------

-- data EXPR = EXPR deriving (Show)

data ANY

data EXPR layout
------------------------
-- === ExprTerm === --
------------------------

data TMP -- FIXME

type    ExprTermDef atom t = N.Term atom (SubLink (Elem (EXPR ANY)) t)
newtype ExprTerm    atom t = ExprTerm    (ExprTermDef atom t)
type    ExprTerm'   atom   = ExprTerm atom TMP
makeWrapped ''ExprTerm




-- === Helpers === --

hideLayout :: ExprTerm atom t -> ExprTerm atom TMP
hideLayout = unsafeCoerce


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
type ValidateScope_ scope sel a = Assert (a `In` TermTypesOf scope) (InvalidFormat sel a scope)


class                                                        ValidateLayout model sel a
instance {-# OVERLAPPABLE #-} ValidateLayout_ model sel a => ValidateLayout model sel a
instance {-# OVERLAPPABLE #-}                                ValidateLayout I     sel a
instance {-# OVERLAPPABLE #-}                                ValidateLayout model I   a
instance {-# OVERLAPPABLE #-}                                ValidateLayout model sel I
type ValidateLayout_ model sel a = ValidateScope (model # sel) sel a
type ValidateLayout' t     sel a = ValidateLayout (t # LAYOUT) sel a


-- === Instances === --

-- FIXME: [WD]: it seems that LAYOUT in the below declaration is something else than real layout - check it and refactor
type instance Access LAYOUT   (ExprTerm atom t) = Access LAYOUT (Unwrapped (ExprTerm atom t))
type instance Access TermType (ExprTerm atom t) = atom
type instance Access Format   (ExprTerm atom t) = Access Format atom
type instance Access TERM     (ExprTerm atom t) = ExprTerm atom t

instance Accessor TERM (ExprTerm atom t) where access = id

instance UncheckedFromTerm (ExprTerm atom t) where uncheckedFromTerm = wrap'

instance ValidateLayout (LayoutOf t) TermType atom
      => FromTerm (ExprTerm atom t) where fromTerm = wrap'


-- Repr
instance Repr s (Unwrapped (ExprTerm atom t))
      => Repr s (ExprTerm atom t) where repr = repr . unwrap'
--
-- -- Fields
-- type instance FieldsType (ExprTerm atom t) = FieldsType (Unwrapped (ExprTerm atom t))
-- instance HasFields (Unwrapped (ExprTerm atom t))
--       => HasFields (ExprTerm atom t) where fieldList = fieldList . unwrap'

-- ModifyFields

-- instance ModifiesFields (Unwrapped (ExprTerm atom t))
--       => ModifiesFields (ExprTerm atom t) where modifyFields f = wrap' . modifyFields f . unwrap'

modifyFields :: Functor (N.TermDef atom) => (SubLink (Elem (EXPR ANY)) t -> SubLink (Elem (EXPR ANY)) t') -> ExprTerm atom t -> ExprTerm atom t'
modifyFields f = wrapped %~ fmap f

-- Inputs
-- type instance InputsType (ExprTerm atom t) = InputsType (Unwrapped (ExprTerm atom t))
-- instance HasInputs (Unwrapped (ExprTerm atom t))
--       => HasInputs (ExprTerm atom t) where inputList = inputList . unwrap'

inputList :: Foldable (N.TermDef atom) => ExprTerm atom t -> [SubLink (Elem (EXPR ANY)) t]
inputList = Foldable.toList . unwrap

-- TermTypeOf
type instance TermTypeOf (ExprTerm atom t) = TermTypeOf (Unwrapped (ExprTerm atom t))

----------------------
-- === ExprData === --
----------------------

type ExprStoreSlots = '[ TermType ':= Enum, Format ':= Mask, TERM ':= Raw ]
type ExprStore = Store ExprStoreSlots

newtype ExprData sys model = ExprData ExprStore deriving (Show)
makeWrapped ''ExprData


-- === Encoding === --

class                                                            TermEncoder atom where encodeTerm :: forall t. ExprTerm atom t -> ExprStore
instance                                                         TermEncoder I    where encodeTerm = impossible
instance EncodeStore ExprStoreSlots (ExprTerm' atom) Identity => TermEncoder atom where
    encodeTerm = runIdentity . encodeStore . hideLayout


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
unsafeRelayout = unsafeCoerce

expr :: forall atom layout m. (TermEncoder atom, MonadRef m, Writer Net AnyExpr m, Emitter (New // AnyExpr) m)
     => ExprTerm atom (Expr layout) -> m (Expr layout)
expr = newElem . encodeTerm

reserveExpr :: (MonadRef m, Writer Net AnyExpr m)
            => m (Expr layout)
reserveExpr = reserveElem

dispatchNewExpr :: (NewElemEvent m (Expr layout), TermEncoder atom) => ExprTerm atom (Expr layout) -> Expr layout -> m ()
dispatchNewExpr = flip dispatchNewElem . encodeTerm

exprs :: (MonadRef m, Reader Net AnyExpr m) => m [SomeExpr]
exprs = uncheckedElems

links :: (MonadRef m, Reader Net (Link' AnyExpr) m) => m [SomeExprLink]
links = uncheckedElems









-- === Instances === --

type instance Sub s     (Expr l) = Expr (Sub s l)


type instance Generalizable (Expr l) (Expr l') = ExprGeneralizable l l'

-- FIXME[WD]: We should re-think how generalizable works, since MK changed it's implementation and there are ugly constraints now.
type ExprGeneralizable' l l' = ExprGeneralizable l l' ~ True
type family ExprGeneralizable l l' where
    ExprGeneralizable l Layout.Bottom = 'True -- FIXME[WD]: shouldn't we introduce `Layoyut` newtype wrapper to indicate that layouts could be always generalized to Any?
    ExprGeneralizable l l'            = Generalizable l l'


-- -------------------------------------
-- === Expr Layout type caches === --
-------------------------------------

type instance Encode TermType    v = List.Index v (Every TermType)
type instance Encode Format  v = List.Index v (Every Format)




-- TO REFACTOR:

type instance UnsafeGeneralizable (Expr l) (Expr l') = ()
type instance UnsafeGeneralizable (Link (Expr l) (Expr r)) (Link (Expr l') (Expr r')) = ()

type family         UnsafeGeneralizable a b :: Constraint
unsafeGeneralize :: UnsafeGeneralizable a b => a -> b
unsafeGeneralize = unsafeCoerce


type instance UnsafeGeneralizable [a] [b] = UnsafeGeneralizable a b



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
unsafeToExprTerm = unsafeCoerce . unwrap' . access @TERM . unwrap' <∘> getLayer @Model

unsafeModifyExprTermDef :: forall atom l m. (MonadRef m, Editor Layer (AnyExpr // Model) m)
                        => Expr l -> (ExprTermDef atom (Expr l) -> ExprTermDef atom (Expr l)) -> m ()
unsafeModifyExprTermDef expr f = do
    oldModel <- getLayer @Model expr
    let oldTerm :: ExprTerm atom (Expr l) = unsafeCoerce $ unwrap' $ access @TERM $ unwrap' oldModel
    let oldDef   = unwrap' oldTerm
    let newModel = wrap' $ update' @TERM (wrap' $ unsafeCoerce $ (wrap' $ f oldDef :: ExprTerm atom (Expr l))) $ unwrap' oldModel
    putLayer @Model expr newModel

unsafeToExprTermDef :: forall atom l m. (MonadRef m, Reader Layer (AnyExpr // Model) m) => Expr l -> m (ExprTermDef atom (Expr l))
unsafeToExprTermDef = unwrap' <∘> unsafeToExprTerm







-- === Term mapping === --
-- | General expr symbol mapping utility. It allows mapping over current symbol in any expr.

class    MonadRef m => TermMapM (atoms :: [*]) ctx expr m b where symbolMapM :: (forall a. ctx a m b => a -> m b) -> expr -> m b
instance MonadRef m => TermMapM '[]            ctx expr m b where symbolMapM _ _ = impossible
instance (  TermMapM as ctx expr m b
         , ctx (ExprTerm a expr) m b
         , idx ~ FromJust (Encode TermType a) -- FIXME: make it nicer and assert
         , KnownNat idx
         , Reader Layer (AnyExpr // Model) m
         , expr ~ Expr layout
         )
      => TermMapM (a ': as) ctx expr m b where
    symbolMapM f expr = do
        d <- unwrap' <$> getLayer @Model expr
        sym <- unsafeToExprTerm @a expr
        let eidx = unwrap' $ access @TermType d
            idx  = fromIntegral $ natVal (Proxy :: Proxy idx)
        if (idx == eidx) then f sym else symbolMapM @as @ctx f expr


type TermMapM_AMB          = TermMapM     (Every TermType)
type TermMapM_AB  ctx      = TermMapM_AMB (DropMonad ctx)
type TermMap_AB   ctx expr = TermMapM_AB  ctx expr Identity
type TermMapM_A   ctx      = TermMapM_AB  (FreeResult ctx)
type TermMap_A    ctx expr = TermMapM_A   ctx expr Identity

symbolMapM_AMB :: forall ctx m expr b. TermMapM_AMB ctx expr m b => (forall a. ctx a m b => a -> m b) -> expr -> m b
symbolMapM_AB  :: forall ctx expr m b. TermMapM_AB  ctx expr m b => (forall a. ctx a   b => a ->   b) -> expr -> m b
symbolMap_AB   :: forall ctx expr   b. TermMap_AB   ctx expr   b => (forall a. ctx a   b => a ->   b) -> expr ->   b
symbolMapM_A   :: forall ctx expr m b. TermMapM_A   ctx expr m b => (forall a. ctx a     => a ->   b) -> expr -> m b
symbolMap_A    :: forall ctx expr   b. TermMap_A    ctx expr   b => (forall a. ctx a     => a ->   b) -> expr ->   b
symbolMapM_AMB   = symbolMapM @(Every TermType) @ctx
symbolMapM_AB  f = symbolMapM_AMB @(DropMonad ctx) (return <$> f)
symbolMap_AB   f = runIdentity . symbolMapM_AB @ctx f
symbolMapM_A     = symbolMapM_AB @(FreeResult ctx)
symbolMap_A    f = runIdentity . symbolMapM_A @ctx f


class ModifiesFields2 a b where
    modifyFields2 :: (Link' SomeExpr -> Link' SomeExpr) -> a -> b

instance (a ~ ExprTerm atom SomeExpr, Functor (N.TermDef atom)) => ModifiesFields2 a Raw where
    modifyFields2 f = wrap' . unsafeCoerce . modifyFields f

withFields :: (TermMapM_AB ModifiesFields2 expr m Raw, expr ~ Expr layout) => (Link' SomeExpr -> Link' SomeExpr) -> expr -> m Raw
withFields f = symbolMapM_AB @ModifiesFields2 (modifyFields2 f)

inplaceModifyFieldsWith :: (Editor Layer (AnyExpr // Model) m, TermMapM_AB ModifiesFields2 expr m Raw, expr ~ Expr layout) => (Link' SomeExpr -> Link' SomeExpr) -> expr -> m ()
inplaceModifyFieldsWith f expr = do
    newModel <- withFields f expr
    modifyLayer_ @Model expr $ wrap' . update' @TERM newModel . unwrap'


-- class    (b ~ [FieldsType a], HasFields a) => HasFields2 a b
-- instance (b ~ [FieldsType a], HasFields a) => HasFields2 a b
--
-- -- WARNING: works only for Drafts for now as it assumes that the child-refs have the same type as the parent
-- -- type FieldsC t layout = TermMap2 HasFields2 (Expr t layout) [Ref (Link (Expr t layout) (Expr t layout))]
-- symbolFields :: (TermMapM_AB HasFields2 expr m out, expr ~ Expr layout, out ~ [Link expr expr]) => expr -> m out
-- symbolFields = symbolMapM_AB @HasFields2 fieldList

class    HasInputs2 a b where inputList2 :: a -> b
instance (a ~ ExprTerm atom t, b ~ [SubLink (Elem (EXPR ANY)) t], Foldable (N.TermDef atom)) => HasInputs2 a b where inputList2 = inputList
inputs :: (TermMapM_AB HasInputs2 expr m out, expr ~ Expr layout, out ~ [SubLink (Elem (EXPR ANY)) t]) => expr -> m out
inputs = symbolMapM_AB @HasInputs2 inputList2

-- inputList :: Foldable (N.TermDef atom) => ExprTerm atom t -> [SubLink (Elem (EXPR ANY)) t]
-- inputList = Foldable.toList . unwrap


class    KnownType (TermTypeOf a) => HasTerm a
instance KnownType (TermTypeOf a) => HasTerm a
termTermDesc :: (TermMapM_A HasTerm expr m out, expr ~ Expr layout, out ~ TermDesc) => expr -> m out
termTermDesc = symbolMapM_A @HasTerm atomDescOf

isSameTerm :: (TermMapM_A HasTerm expr m out, expr ~ Expr layout, out ~ TermDesc) => expr -> expr -> m Bool
isSameTerm a b = (==) <$> termTermDesc a <*> termTermDesc b

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

-- FIXME[WD -> MK]: This should be implemented for any element accessible in the graph, not only for exprs and links.
-- FIXME: This should be together with other Payloads, but uses Expr and ExprLink which are not visible at that place in the file

data ElemTranslations = ElemTranslations { _exprTranslator :: forall l.   Expr l       -> Expr l
                                         , _linkTranslator :: forall a b. ExprLink a b -> ExprLink a b
                                         }
makeLenses ''ElemTranslations

instance IsPayload   (Import // t) where
    type PayloadData (Import // t) = (t, ElemTranslations, [LayerRep])

instance IsPayload   (OnQueryChildren // t) where
    type PayloadData (OnQueryChildren // t) = (t, Store.UnsafeSTRef [SomeExprLink])

instance IsPayload   (OnDeepDelete // t) where
    type PayloadData (OnDeepDelete // t) = (t, Set SomeExpr)

deepDelete :: forall l m. Event.Emitter (OnDeepDelete // AnyExpr) m => Expr l -> m ()
deepDelete t = emit $ Payload @(OnDeepDelete // Expr l) (t, Set.empty)

deepDeleteWithWhitelist :: forall l m. Event.Emitter (OnDeepDelete // AnyExpr) m => Expr l -> Set SomeExpr -> m ()
deepDeleteWithWhitelist t s = emit $ Payload @(OnDeepDelete // Expr l) (t, s)

queryChildren :: forall l m. (MonadRef m, Event.Emitter (OnQueryChildren // AnyExpr) m) => Expr l -> m [SomeExprLink]
queryChildren t = do
    ref <- Store.newUnsafeSTRef []
    emit $ Payload @(OnQueryChildren // Expr l) (t, ref)
    Store.readUnsafeSTRef ref
