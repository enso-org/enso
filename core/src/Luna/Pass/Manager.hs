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
import Luna.Pass.Class (MonadPassManager_Boot, SubPass, PassRep, Proto, DynPass, Template, Uninitialized)
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

data LayerReg m = LayerReg { _prototypes :: Map LayerRep $ Proto (Pass.Describbed (Uninitialized m (Template (DynPass m))))
                           , _attached   :: Map ElemRep  $ Map LayerRep $ PassRep
                           } deriving (Show)

data State m = State { _passes :: Map PassRep (Pass.Describbed (Uninitialized m (DynPass m)))
                     , _layers :: LayerReg m
                     , _events :: EventHub SortedListenerRep (Pass.Describbed (Uninitialized m (Template (DynPass m))))
                     , _attrs  :: Map AttrRep Prim.AnyData
                     } deriving (Show)

type RefState m = State (GetRefHandler m)

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

type instance MonadPassManager_Boot m = MonadPassManager m
class (IRMonad m, MonadRef m) => MonadPassManager m where
    get :: m (RefState m)
    put :: RefState m -> m ()

instance IRMonad m => MonadPassManager (PassManager m) where
    get = wrap'   State.get ; {-# INLINE get #-}
    put = wrap' . State.put ; {-# INLINE put #-}

type TransBaseMonad t m = (MonadTransInvariants' t m, IRMonad (t m), MonadRef (t m), GetRefHandler m ~ GetRefHandler (t m))
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

registerLayerProto :: MonadPassManager m => LayerRep -> Proto (Pass.Describbed (Uninitialized (GetRefHandler m) (Template (DynPass (GetRefHandler m))))) -> m ()
registerLayerProto l f = withDebug ("Registering layer " <> show l) $ modify_ $ layers . prototypes . at l ?~ f ; {-# INLINE registerLayerProto #-}

-- FIXME[WD]: pass manager should track pass deps so priority should be obsolete in the future!
attachLayer :: MonadPassManager m => Int -> LayerRep -> ElemRep -> m ()
attachLayer priority l e = withDebug ("Attaching " <> show e <> " layer " <> show l) $ do
    IR.unsafeCreateNewLayer l e
    s <- get
    let Just pproto = s ^. layers . prototypes . at l
        dpass = Pass.specialize pproto e
        s' = s & layers . attached . at e . non Map.empty . at l ?~ (dpass ^. Pass.desc . Pass.passRep)
    put s'
    print $ ">>> " <> show dpass
    addEventListener priority (New // (e ^. asTypeRep)) dpass -- TODO
    -- TODO: register new available pass!
{-# INLINE attachLayer #-}

queryListeners :: MonadPassManager m => Event.Tag -> m [Uninitialized (GetRefHandler m) (Template (DynPass (GetRefHandler m)))]
queryListeners t = fmap (view Pass.content) . Event.queryListeners t . view events <$> get ; {-# INLINE queryListeners #-}


addEventListener :: (MonadPassManager m, IsTag evnt)
                  => Int -> evnt -> (Pass.Describbed (Uninitialized (GetRefHandler m) (Template (DynPass (GetRefHandler m))))) -> m ()
addEventListener priority evnt p =  modify_ $ (events %~ attachListener (Listener (toTag evnt) $ SortedListenerRep priority $ switchRep rep) p)
    where rep = p ^. Pass.desc . Pass.passRep
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



-- class ContainsRef pass k a m where findRef :: Lens' (DataSet m pass) (Ref k a m)


type instance RefData EVENT _ m = Template (DynPass m)


instance (MonadPassManager m, Pass.ContainsRef pass EVENT e (GetRefHandler m))
      => Emitter (SubPass pass m) e where
    emit (Event p) = do
        tmpl <- unwrap' . view (Pass.findRef @pass @EVENT @e) <$> Pass.get
        liftRefHandler $ Pass.runDynPass $ Pass.unsafeInstantiate p tmpl
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




type instance GetRefHandler (PassManager m) = PassManager m
instance IRMonad m => MonadRef (PassManager m) where
    liftRefHandler = id ; {-# INLINE liftRefHandler #-}




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


-- type instance GetRefHandler (RefCache m) = GetRefHandler m
-- instance MonadPass m => MonadPass (RefCache m) where
--     liftRefHandler = lift . liftRefHandler ; {-# INLINE liftRefHandler #-}

instance MonadLogging m => MonadLogging (RefCache m)
--
-- instance (MonadRefLookup k m, KnownType k, MonadFix m, MonadIO m) => MonadRefLookup k (RefCache m) where
--     uncheckedLookupRef keyRep = mdo
--         let ckey = (typeVal' @k, keyRep)
--         print ("caching " <> show ckey)
--         c <- getCache
--         case c ^. at ckey of
--             Just v  -> return (unsafeCoerce v)
--             Nothing -> mdo
--                 putCache $ c & at ckey .~ Just (unsafeCoerce out)
--                 out <- lift $ uncheckedLookupRef keyRep
--                 return out
        -- lift $ uncheckedLookupRef keyRep

-- class Monad m => MonadRefLookup k m where
--     uncheckedLookupRef :: forall a. TypeRep {- the type of `a` -}
--                        -> m (Maybe (Ref k a m))


-- Ref nie powinien byc zalezny od m - w ogole, tylko od PrimState s = to jest gwarancja dla PassManagera
-- trzebaby zrobic poza tym listeners, tak by listener zalezny byl od monady passmanagera, a nie tak jak teraz klucz od monady pod monada passu.
-- Wtedy mozna robic Cache dla eventlsitenerow, teraz sie nie da.
--
-- + renaming
-- Uninitialized -> Uninitialized
-- initialize -> compile / compileTemplate
--
-- initialize -> initialize

instance (IRMonad m, MonadRefCache m) => MonadRefLookup Attr (PassManager m) where
    uncheckedLookupRef a = fmap unsafeCoerce . (^? (attrs . ix (fromTypeRep a))) <$> get ; {-# INLINE uncheckedLookupRef #-}


instance IRMonad m => MonadRefLookup LAYER (PassManager m) where
    uncheckedLookupRef a = do
        s <- getIR
        let (_,[e,l]) = splitTyConApp a -- dirty typrep of (Layer e l) extraction
            mlv = s ^? wrapped' . ix e
        mr <- liftRefHandler $ mapM (Store.readKey l) mlv
        return $ wrap' <$> join mr
    {-# INLINE uncheckedLookupRef #-}

instance IRMonad m => MonadRefLookup Net (PassManager m) where
    uncheckedLookupRef a = fmap wrap' . (^? (wrapped' . ix a)) <$> liftRefHandler getIR ; {-# INLINE uncheckedLookupRef #-}


instance (IRMonad m, MonadRefCache m) => MonadRefLookup EVENT (PassManager m) where -- Event.FromPath e
    uncheckedLookupRef a = do
        let ckey = (typeVal' @EVENT, a)
        c <- getCache
        case c ^. at ckey of
            Just v  -> do
                return $ Just $ unsafeCoerce v
            Nothing -> mdo
                putCache $ c & at ckey .~ Just (unsafeCoerce $ fromJust out) -- this fromJust is safe. It will be used as a cache only if everything else succeeds
                out <- (fmap . fmap) (Ref . fmap sequence_ . sequence) . fmap (fimxe2 . sequence) . sequence . fmap Pass.initialize =<< queryListeners (Event.fromPathDyn a)
                return out


fimxe2 = \case
    Left e -> Nothing
    Right a -> Just a
