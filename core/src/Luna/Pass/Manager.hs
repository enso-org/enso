{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances    #-}

module Luna.Pass.Manager where

import Luna.Prelude

import qualified Control.Monad.State as State
import           Control.Monad.State (StateT, evalStateT, runStateT)
import qualified Data.Map            as Map
import           Data.Map            (Map)
import           Data.Event          (EVENT, Event(Event), EventHub, Emitter, IsTag, Listener(Listener), toTag, attachListener, (//))
import qualified Data.Event          as Event

import Luna.IR.Internal.IR as IR
import Luna.Pass.Class (DynSubPass, SubPass, PassRep, DynPassTemplate, Proto, DynPass3, Template, Uninitialized)
import qualified Luna.Pass.Class as Pass

import qualified Prologue.Prim as Prim
import Data.TypeVal

import System.Log
import qualified Luna.IR.Internal.LayerStore as Store




type Cache = Map (TypeRep,TypeRep) Prim.Any
class Monad m => MonadRefCache m where
    getCache :: m Cache
    putCache :: Cache -> m ()


data SortedListenerRep = SortedListenerRep Int Event.ListenerRep deriving (Show, Eq)
instance Ord SortedListenerRep where
    compare (SortedListenerRep i r) (SortedListenerRep i' r') = if i == i' then compare r r' else compare i i'



-------------------
-- === State === --
-------------------

data LayerReg m = LayerReg { _prototypes :: Map LayerRep $ Proto (Pass.Describbed (Uninitialized m (Template (DynPass3 m))))
                           , _attached   :: Map ElemRep  $ Map LayerRep $ PassRep
                           } deriving (Show)

data State m = State { _passes :: Map PassRep (Pass.Describbed (Uninitialized m (DynPass3 m)))
                     , _layers :: LayerReg m
                     , _events :: EventHub SortedListenerRep (Pass.Describbed (Uninitialized m (Template (DynPass3 m))))
                     , _attrs  :: Map AttrRep Prim.AnyData
                     } deriving (Show)

type RefState m = State (GetPassHandler m)

makeLenses ''State
makeLenses ''LayerReg


-- === instances === --

instance Default (LayerReg m) where def = LayerReg def def         ; {-# INLINE def #-}
instance Default (State    m) where def = State    def def def def ; {-# INLINE def #-}



--------------------------
-- === Pass Manager === --
--------------------------

-- === Definition === --

newtype PassManager m a = PassManager (StateT (State (PassManager m)) m a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadThrow)
makeWrapped ''PassManager


-- === MonadPassManager === --

class (IRMonad m, IR.MonadPass m) => MonadPassManager m where
    get :: m (RefState m)
    put :: RefState m -> m ()

instance IRMonad m => MonadPassManager (PassManager m) where
    get = wrap'   State.get ; {-# INLINE get #-}
    put = wrap' . State.put ; {-# INLINE put #-}

type TransBaseMonad t m = (MonadTransInvariants' t m, IRMonad (t m), IR.MonadPass (t m), GetPassHandler m ~ GetPassHandler (t m))
instance {-# OVERLAPPABLE #-} (MonadPassManager m, TransBaseMonad t m)
      => MonadPassManager (t m) where
    get = lift   get ; {-# INLINE get #-}
    put = lift . put ; {-# INLINE put #-}

modifyM :: MonadPassManager m => (RefState m -> m (a, RefState m)) -> m a
modifyM f = do
    s <- get
    (a, s') <- f s
    put s'
    return a
{-# INLINE modifyM #-}

modifyM_ :: MonadPassManager m => (RefState m -> m (RefState m)) -> m ()
modifyM_ = modifyM . fmap (fmap ((),)) ; {-# INLINE modifyM_ #-}

modify_ :: MonadPassManager m => (RefState m -> RefState m) -> m ()
modify_ = modifyM_ . fmap return ; {-# INLINE modify_ #-}


-- === Running === --

evalPassManager :: Logging m => PassManager m a -> State (PassManager m) -> m a
evalPassManager = withDebugBy "PassManager" "Running PassManager" .: evalStateT . unwrap' ; {-# INLINE evalPassManager #-}

evalPassManager' :: Logging m => PassManager m a -> m a
evalPassManager' = flip evalPassManager def ; {-# INLINE evalPassManager' #-}


-- === Utils === --

registerLayerProto :: MonadPassManager m => LayerRep -> Proto (Pass.Describbed (Uninitialized (GetPassHandler m) (Template (DynPass3 (GetPassHandler m))))) -> m ()
registerLayerProto l f = withDebug ("Registering layer " <> show l) $ modify_ $ layers . prototypes . at l ?~ f ; {-# INLINE registerLayerProto #-}

-- FIXME[WD]: pass manager should track pass deps so priority should be obsolete in the future!
attachLayerPM2 :: MonadPassManager m => Int -> LayerRep -> ElemRep -> m ()
attachLayerPM2 priority l e = withDebug ("Attaching " <> show e <> " layer " <> show l) $ do
    s <- get
    let Just pproto = s ^. layers . prototypes . at l
        dpass = Pass.specialize pproto e
        s' = s & layers . attached . at e . non Map.empty . at l ?~ (dpass ^. Pass.desc2 . Pass.passRep)
    put s'
    addEventListener priority (NEW // (e ^. asTypeRep)) dpass -- TODO
    -- TODO: register new available pass!
{-# INLINE attachLayerPM2 #-}

queryListeners :: MonadPassManager m => Event.Tag -> m [Uninitialized (GetPassHandler m) (Template (DynPass3 (GetPassHandler m)))]
queryListeners t = fmap (view Pass.content) . Event.queryListeners t . view events <$> get ; {-# INLINE queryListeners #-}


addEventListener :: (MonadPassManager m, IsTag evnt)
                  => Int -> evnt -> (Pass.Describbed (Uninitialized (GetPassHandler m) (Template (DynPass3 (GetPassHandler m))))) -> m ()
addEventListener priority evnt p =  modify_ $ (events %~ attachListener (Listener (toTag evnt) $ SortedListenerRep priority $ switchRep rep) cp)
    where cp  = p -- Pass.compile $ Pass.dropResult p
          rep = cp ^. Pass.desc2 . Pass.passRep
{-# INLINE addEventListener #-}



-- === Instances === --

-- Logging
instance MonadLogging m => MonadLogging (PassManager  m)

-- Primitive
instance PrimMonad m => PrimMonad (PassManager m) where
    type PrimState (PassManager m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}

instance MonadTrans PassManager where
    lift = wrap' . lift ; {-# INLINE lift #-}



-- class ContainsKey pass k a m where findKey :: Lens' (DataSet m pass) (Key k a m)


type instance KeyData EVENT _ m = Template (DynPass3 m)


instance (MonadPassManager m, Pass.ContainsKey pass EVENT e (GetPassHandler m))
      => Emitter (SubPass pass m) e where
    emit (Event p) = do
        tmpl <- unwrap' . view (Pass.findKey @pass @EVENT @e) <$> Pass.get
        liftPassHandler $ Pass.runDynPass $ Pass.unsafeInstantiate p tmpl
--
--
-- class Monad m => Emitter m a where
--     emit :: forall e. a ~ Abstract e => Event e -> m ()

-- class Monad m => Emitter m a where
--     emit :: forall e. a ~ Abstract e => Proxy e -> Payload e -> m ()
--
-- instance Emitter (SubPass pass m) e where
--     emit _ p =




unsafeWriteAttr :: MonadPassManager m => AttrRep -> a -> m ()
unsafeWriteAttr r a = modify_ $ attrs %~ Map.insert r (unsafeCoerce a)

unsafeReadAttr :: MonadPassManager m => AttrRep -> m (Maybe Prim.AnyData)
unsafeReadAttr r = view (attrs . at r) <$> get

unsafeSetAttr :: MonadPassManager m => AttrRep -> Maybe Prim.AnyData -> m ()
unsafeSetAttr r a = modify_ $ attrs . at r .~ a

unsafeDeleteAttr :: MonadPassManager m => AttrRep -> m ()
unsafeDeleteAttr r = modify_ $ attrs %~ Map.delete r

unsafeWithAttr :: MonadPassManager m => AttrRep -> a -> m t -> m t
unsafeWithAttr r a f = do
    oldAttr <- unsafeReadAttr r
    unsafeWriteAttr r a
    out <- f
    unsafeSetAttr r oldAttr
    return out




type instance GetPassHandler (PassManager m) = PassManager m
instance IRMonad m => MonadPass (PassManager m) where
    liftPassHandler = id




----------------------
-- === RefCache === --
----------------------

newtype RefCache m a = RefCache (StateT Cache m a) deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadFix, MonadThrow)
makeWrapped ''RefCache

runRefCache :: Monad m => RefCache m a -> m a
runRefCache = flip evalStateT mempty . unwrap' ; {-# INLINE runRefCache #-}




instance Monad m => MonadRefCache (RefCache m) where
    getCache = wrap'   State.get
    putCache = wrap' . State.put

instance {-# OVERLAPPABLE #-} (Monad m, Monad (t m), MonadTrans t, MonadRefCache m) => MonadRefCache (t m) where
    getCache = lift   getCache
    putCache = lift . putCache


-- === Instances === --

-- Primitive
instance PrimMonad m => PrimMonad (RefCache m) where
    type PrimState (RefCache m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}


-- type instance GetPassHandler (RefCache m) = GetPassHandler m
-- instance MonadPass m => MonadPass (RefCache m) where
--     liftPassHandler = lift . liftPassHandler ; {-# INLINE liftPassHandler #-}

instance MonadLogging m => MonadLogging (RefCache m)
--
-- instance (KeyMonad k m, KnownType k, MonadFix m, MonadIO m) => KeyMonad k (RefCache m) where
--     uncheckedLookupKey keyRep = mdo
--         let ckey = (typeVal' @k, keyRep)
--         print ("caching " <> show ckey)
--         c <- getCache
--         case c ^. at ckey of
--             Just v  -> return (unsafeCoerce v)
--             Nothing -> mdo
--                 putCache $ c & at ckey .~ Just (unsafeCoerce out)
--                 out <- lift $ uncheckedLookupKey keyRep
--                 return out
        -- lift $ uncheckedLookupKey keyRep

-- class Monad m => KeyMonad k m where
--     uncheckedLookupKey :: forall a. TypeRep {- the type of `a` -}
--                        -> m (Maybe (Key k a m))


-- Key nie powinien byc zalezny od m - w ogole, tylko od PrimState s = to jest gwarancja dla PassManagera
-- trzebaby zrobic poza tym listeners, tak by listener zalezny byl od monady passmanagera, a nie tak jak teraz klucz od monady pod monada passu.
-- Wtedy mozna robic Cache dla eventlsitenerow, teraz sie nie da.
--
-- + renaming
-- Uninitialized -> Uninitialized
-- initialize -> compile / compileTemplate
--
-- initialize -> initialize

instance (IRMonad m, MonadRefCache m) => KeyMonad ATTR (PassManager m) where
    uncheckedLookupKey a = fmap unsafeCoerce . (^? (attrs . ix (fromTypeRep a))) <$> get ; {-# INLINE uncheckedLookupKey #-}


instance IRMonad m => KeyMonad LAYER (PassManager m) where
    uncheckedLookupKey a = do
        s <- getIR
        let (_,[e,l]) = splitTyConApp a -- dirty typrep of (Layer e l) extraction
            mlv = s ^? wrapped' . ix e
        mr <- liftPassHandler $ mapM (Store.readKey l) mlv
        return $ wrap' <$> join mr
    {-# INLINE uncheckedLookupKey #-}

instance IRMonad m => KeyMonad NET (PassManager m) where
    uncheckedLookupKey a = fmap wrap' . (^? (wrapped' . ix a)) <$> liftPassHandler getIR ; {-# INLINE uncheckedLookupKey #-}


instance (IRMonad m, MonadRefCache m) => KeyMonad EVENT (PassManager m) where -- Event.FromPath e
    uncheckedLookupKey a = do
        let ckey = (typeVal' @EVENT, a)
        c <- getCache
        case c ^. at ckey of
            Just v  -> do
                return $ Just $ unsafeCoerce v
            Nothing -> mdo
                putCache $ c & at ckey .~ Just (unsafeCoerce $ fromJust out) -- this fromJust is safe. It will be used as a cache only if everything else succeeds
                out <- (fmap . fmap) (Key . fmap sequence_ . sequence) . fmap (fimxe2 . sequence) . sequence . fmap Pass.initialize =<< queryListeners (Event.fromPathDyn a)
                return out


fimxe2 = \case
    Left e -> Nothing
    Right a -> Just a
