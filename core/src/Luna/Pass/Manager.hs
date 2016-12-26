{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances    #-}

module Luna.Pass.Manager where

import Luna.Prelude

import qualified Control.Monad.State as State
import           Control.Monad.State (StateT, evalStateT, runStateT)
import qualified Data.Map            as Map
import           Data.Map            (Map)
import           Data.Event          (EVENT, Event(Event), EventHub, Emitter, Emitter2, IsTag, Listener(Listener), toTag, attachListener, (//))
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



-------------------
-- === State === --
-------------------

data LayerReg m = LayerReg { _prototypes :: Map LayerRep $ Proto (Pass.Describbed (Uninitialized (PassManager m) (Template (DynPass3 (PassManager m)))))
                           , _attached   :: Map ElemRep  $ Map LayerRep $ PassRep
                           } deriving (Show)

data State m = State { _passes :: Map Int Int -- Map PassRep $ PMPass m
                     , _layers :: LayerReg m
                     , _events :: EventHub SortedListenerRep (Pass.Describbed (Uninitialized (PassManager m) (Template (DynPass3 (PassManager m)))))
                     , _attrs  :: Map AttrRep Prim.AnyData
                     } deriving (Show)


data SortedListenerRep = SortedListenerRep Int Event.ListenerRep deriving (Show, Eq)
instance Ord SortedListenerRep where
    compare (SortedListenerRep i r) (SortedListenerRep i' r') = if i == i' then compare r r' else compare i i'


-- === instances === --

instance Default (LayerReg m) where
    def = LayerReg def def ; {-# INLINE def #-}

instance Default (State m) where
    def = State def def def def ; {-# INLINE def #-}


--------------------------
-- === Pass Manager === --
--------------------------

-- === Definition === --

newtype PassManager m a = PassManager (StateT (State m) m a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadThrow)

makeLenses  ''LayerReg
makeLenses  ''State
makeWrapped ''PassManager

type GetPassManager m = PassManager (GetBaseMonad m)

type family GetBaseMonad m where
    GetBaseMonad (PassManager m) = m
    GetBaseMonad (t m)           = GetBaseMonad m

type State'  m = State  (GetBaseMonad m)

class (IRMonad m, IRMonad (GetBaseMonad m), MonadRefCache (GetBaseMonad m)) => MonadPassManager m where
    get :: m (State' m)
    put :: State' m -> m ()
    liftPassManager :: GetPassManager m a -> m a

instance (IRMonad m, MonadRefCache m) => MonadPassManager (PassManager m) where
    get = wrap'   State.get ; {-# INLINE get #-}
    put = wrap' . State.put ; {-# INLINE put #-}
    liftPassManager = id

type TransBaseMonad t m = (GetBaseMonad m ~ GetBaseMonad (t m), MonadTransInvariants' t m, IRMonad (t m))
instance {-# OVERLAPPABLE #-} (MonadPassManager m, TransBaseMonad t m)
      => MonadPassManager (t m) where
    get = lift   get ; {-# INLINE get #-}
    put = lift . put ; {-# INLINE put #-}
    liftPassManager = lift . liftPassManager


modifyM :: MonadPassManager m => (State' m -> m (a, State' m)) -> m a
modifyM f = do
    s <- get
    (a, s') <- f s
    put s'
    return a
{-# INLINE modifyM #-}

modifyM_ :: MonadPassManager m => (State' m -> m (State' m)) -> m ()
modifyM_ = modifyM . fmap (fmap ((),)) ; {-# INLINE modifyM_ #-}

modify_ :: MonadPassManager m => (State' m -> State' m) -> m ()
modify_ = modifyM_ . fmap return ; {-# INLINE modify_ #-}


-- === Running === --

evalPassManager :: Logging m => PassManager m a -> State m -> m a
evalPassManager = withDebugBy "PassManager" "Running PassManager" .: evalStateT . unwrap' ; {-# INLINE evalPassManager #-}

evalPassManager' :: Logging m => PassManager m a -> m a
evalPassManager' = flip evalPassManager def ; {-# INLINE evalPassManager' #-}


-- === Utils === --

-- FIXME[WD]: pass manager should track pass deps so priority should be obsolete in the future!
-- attachLayerPM :: (MonadPassManager m, Functor (GetBaseMonad m)) => Int -> LayerRep -> ElemRep -> m ()
-- attachLayerPM priority l e = do
--     s <- get
--     let Just lcons = s ^. layers . prototypes_old . at l
--         lpass = appCons lcons e
--         s' = s & layers . attached . at e . non Map.empty . at l ?~ (lpass ^. Pass.desc . Pass.repr)
--     put s'
--     addEventListener priority (NEW // (e ^. asTypeRep)) lpass -- TODO
--     -- TODO: register new available pass!
-- {-# INLINE attachLayerPM #-}

-- FIXME[WD]: make forall t. like security check for layers registration
-- registerLayer :: MonadPassManager m => LayerRep -> (TypeRep -> PMPass' m) -> m ()
-- registerLayer l f = modify_ $ layers . prototypes_old . at l ?~ PassProto_old f ; {-# INLINE registerLayer #-}

registerLayerProto :: MonadPassManager m => LayerRep -> Proto (Pass.Describbed (Uninitialized (GetPassManager m) (Template (DynPass3 (GetPassManager m))))) -> m ()
registerLayerProto l f = withDebug ("Registering layer " <> show l) $ modify_ $ layers . prototypes . at l ?~ f ; {-# INLINE registerLayerProto #-}

-- FIXME[WD]: pass manager should track pass deps so priority should be obsolete in the future!
attachLayerPM2 :: (MonadPassManager m, Functor (GetBaseMonad m))
               => Int -> LayerRep -> ElemRep -> m ()
attachLayerPM2 priority l e = withDebug ("Attaching " <> show e <> " layer " <> show l) $ do
    s <- get
    let Just pproto = s ^. layers . prototypes . at l
        dpass = Pass.specialize pproto e
        s' = s & layers . attached . at e . non Map.empty . at l ?~ (dpass ^. Pass.desc2 . Pass.passRep)
    put s'
    addEventListener2 priority (NEW2 // (e ^. asTypeRep)) dpass -- TODO
    -- TODO: register new available pass!
{-# INLINE attachLayerPM2 #-}

-- (Pass.Describbed (Uninitialized (PassManager m) (Template (DynPass3 (PassManager m)))))

-- queryListeners :: MonadPassManager m => Event.Tag -> m [PMPass' m]
-- queryListeners t = do
--     s <- get
--     let pss  = s ^. passes
--         evs  = Event.queryListeners t $ s ^. events_old
--         lsts = map (fromJust . flip Map.lookup pss) evs
--         fromJust (Just a) = a
--     return lsts

queryListeners2 :: MonadPassManager m => Event.Tag -> m [Uninitialized (GetPassManager m) (Template (DynPass3 (GetPassManager m)))]
queryListeners2 t = do
    s <- get
    let pss  = s ^. passes
        evs  = Event.queryListeners t $ s ^. events
    return (view Pass.content <$> evs)

--

-- FIXME[WD]: pass manager should track pass deps so priority should be obsolete in the future!
-- addEventListener :: (MonadPassManager m, IsTag evnt, IsPass (GetPassManager m) p) => Int -> evnt -> p (GetPassManager m) a -> m ()
-- addEventListener priority evnt p = modify_ $ (events_old %~ attachListener (Listener (toTag evnt) $ SortedListenerRep priority $ switchRep rep) rep)
--                                            . (passes %~ Map.insert rep cp)
--     where cp  = Pass.compile $ Pass.dropResult p
--           rep = cp ^. Pass.desc . Pass.repr
-- {-# INLINE addEventListener #-}

addEventListener2 :: (MonadPassManager m, IsTag evnt)
                  => Int -> evnt -> (Pass.Describbed (Uninitialized (GetPassManager m) (Template (DynPass3 (GetPassManager m))))) -> m ()
addEventListener2 priority evnt p =  modify_ $ (events %~ attachListener (Listener (toTag evnt) $ SortedListenerRep priority $ switchRep rep) cp)
    where cp  = p -- Pass.compile $ Pass.dropResult p
          rep = cp ^. Pass.desc2 . Pass.passRep
{-# INLINE addEventListener2 #-}


-- registerGenericLayer :: l -> (t -> Definition t -> m (LayerData UID t)) -> m ()
-- registerGenericLayer _


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


type instance KeyData EVENT _ m = Template (DynPass3 (GetPassManager m))


instance (MonadPassManager m, Pass.ContainsKey pass EVENT e (GetPassHandler m)
         , GetBaseMonad (GetPassHandler m) ~ GetBaseMonad m) -- FIXME[WD]: this should not be needed
      => Emitter2 (SubPass pass m) e where
    emit2 (Event p) = do
        tmpl <- unwrap' . view (Pass.findKey @pass @EVENT @e) <$> Pass.get
        liftPassManager $ Pass.runDynPass $ Pass.unsafeInstantiate p tmpl
--
--
-- class Monad m => Emitter2 m a where
--     emit2 :: forall e. a ~ Abstract e => Event e -> m ()

-- class Monad m => Emitter2 m a where
--     emit2 :: forall e. a ~ Abstract e => Proxy e -> Payload e -> m ()
--
-- instance Emitter2 (SubPass pass m) e where
--     emit2 _ p =




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
                out <- (fmap . fmap) (Key . fmap sequence_ . sequence) . fmap (fimxe2 . sequence) . sequence . fmap Pass.initialize =<< queryListeners2 (Event.fromPathDyn a)
                return out


fimxe2 = \case
    Left e -> Nothing
    Right a -> Just a
