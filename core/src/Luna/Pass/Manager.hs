{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances    #-}

module Luna.Pass.Manager where

import Luna.Prelude

import qualified Control.Monad.State as State
import           Control.Monad.State (StateT, evalStateT, runStateT)
import qualified Data.Map            as Map
import           Data.Map            (Map)
import qualified Data.Set            as Set
import           Data.Set            (Set)
import           Data.Event          (Event, Payload(Payload), EventHub, Emitter, Listener(Listener), Tag(Tag), attachListener)
import qualified Data.Event          as Event

import Luna.IR.Internal.IR as IR
import Luna.Pass.Class (SubPass, PassRep, Proto, DynPass, DynSubPass(DynSubPass), Template, Uninitialized, PassConstruction)
import qualified Luna.Pass.Class as Pass

import qualified Prologue.Prim as Prim
import Data.TypeDesc

import System.Log
import qualified Luna.IR.Internal.LayerStore as Store
import Luna.IR.Layer



type Cache = Map (TypeDesc,TypeDesc) Prim.Any
class Monad m => MonadRefCache m where
    getCache :: m Cache
    putCache :: Cache -> m ()


data SortedListenerRep = SortedListenerRep Int Event.ListenerRep deriving (Show, Eq)
instance Ord SortedListenerRep where
    compare (SortedListenerRep i r) (SortedListenerRep i' r') = if i == i' then compare r r' else compare i i'



-------------------
-- === State === --
-------------------


data State m = State { _passes    :: Map PassRep (Pass.Describbed (Uninitialized m (DynPass m))) -- not used yet
                     , _layers    :: Map ElemRep  $ Map LayerRep PassRep                         -- not used yet
                     , _listeners :: ListenerState m
                     , _attrs     :: Map AttrRep Prim.AnyData
                     } deriving (Show)

data ListenerState m = ListenerState { _decls :: Map LayerRep $ Map PassRep $ ListenerDecl m
                                     , _hub   :: EventHub SortedListenerRep (Pass.Describbed (Uninitialized m (Template (DynPass m))))
                                     } deriving (Show)

data ListenerDecl m = GenericListener  (Proto $ Event.Tagged (Pass.Describbed (Uninitialized m (Template (DynPass m)))))
                    | SpecificListener ElemRep (Event.Tagged (Pass.Describbed (Uninitialized m (Template (DynPass m)))))
                    deriving (Show)

type RefState m = State (GetRefHandler m)

makeLenses ''State
makeLenses ''ListenerState


-- === Utils === --

specializeListener :: ElemRep -> ListenerDecl m -> Maybe (Event.Tagged (Pass.Describbed (Uninitialized m (Template (DynPass m)))))
specializeListener e = \case
    GenericListener  p    -> Just $ Pass.specialize p e
    SpecificListener le p -> guarded (le == e) p
{-# INLINE specializeListener #-}


-- === instances === --

instance Default (ListenerState m) where def = ListenerState def def         ; {-# INLINE def #-}
instance Default (State         m) where def = State         def def def def ; {-# INLINE def #-}



--------------------------
-- === Pass Manager === --
--------------------------

-- === Definition === --

newtype PassManager m a = PassManager (StateT (State (PassManager m)) m a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadThrow)
makeWrapped ''PassManager


-- === MonadPassManager === --
type MonadPassManager m = (MonadPassManager' m, MonadPassManager' (GetRefHandler m))

class (MonadIR m, MonadRef m, PassConstruction m) => MonadPassManager' m where
    get :: m (RefState m)
    put :: RefState m -> m ()

instance (MonadIR m, MonadRefCache m) => MonadPassManager' (PassManager m) where
    get = wrap'   State.get ; {-# INLINE get #-}
    put = wrap' . State.put ; {-# INLINE put #-}

type TransBaseMonad t m = (MonadTransInvariants' t m, MonadRef (t m), GetRefHandler m ~ GetRefHandler (t m))
instance {-# OVERLAPPABLE #-} (MonadPassManager m, TransBaseMonad t m, PassConstruction (t m))
      => MonadPassManager' (t m) where
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

-- FIXME[WD]: torefactor
emptyMap :: Prism' (Map k a) ()
emptyMap = prism' (\() -> Map.empty) $ guard . Map.null
{-# INLINE emptyMap #-}

-- FIXME[WD]: torefactor
subMapAt t = non' emptyMap . at t


registerElemEventListener :: MonadPassManager m => LayerRep -> PassRep -> ListenerDecl (GetRefHandler m) -> m ()
registerElemEventListener l p f = modify_ $ listeners . decls . at l . subMapAt p ?~ f
    -- where f' = flip (fmap . fmap) f $ \df -> (fmap . fmap . fmap) (withDebugBy ("Pass [" <> show (df ^. Pass.desc . Pass.passRep) <> "]") "Running") df
{-# INLINE registerElemEventListener #-}

registerGenericElemEventListener :: MonadPassManager m => LayerRep -> PassRep -> Proto (Event.Tagged (Pass.Describbed (Uninitialized (GetRefHandler m) (Template (DynPass (GetRefHandler m)))))) -> m ()
registerGenericElemEventListener l p f = withDebug ("Registering event listener " <> show p <> " (* // " <> show l <> ")") $ registerElemEventListener l p (GenericListener f')
    where f' = flip (fmap . fmap) f $ \df -> (fmap . fmap . fmap) (withDebugBy ("Pass [" <> show (df ^. Pass.desc . Pass.passRep) <> "]") "Running") df
{-# INLINE registerGenericElemEventListener #-}

registerSpecificElemEventListener :: MonadPassManager m => ElemRep -> LayerRep -> PassRep -> Event.Tagged (Pass.Describbed (Uninitialized (GetRefHandler m) (Template (DynPass (GetRefHandler m))))) -> m ()
registerSpecificElemEventListener e l p f = withDebug ("Registering event listener " <> show p <> " (" <> show e <> " // " <> show l <> ")") $ registerElemEventListener l p (SpecificListener e f')
    where f' = flip fmap f $ \df -> (fmap . fmap . fmap) (withDebugBy ("Pass [" <> show (df ^. Pass.desc . Pass.passRep) <> "]") "Running") df
{-# INLINE registerSpecificElemEventListener #-}

-- registerElemEventListener' :: MonadPassManager m => LayerRep -> PassRep -> Proto (Event.Tagged (Pass.Describbed (Uninitialized (GetRefHandler m) (Template (DynPass (GetRefHandler m)))))) -> m ()
-- registerElemEventListener' = registerElemEventListener Nothing ; {-# INLINE registerElemEventListener' #-}

-- registerElemEventListener2 :: MonadPassManager m => LayerRep -> Proto (Pass.Describbed (Uninitialized (GetRefHandler m) (Template (DynPass (GetRefHandler m))))) -> m ()
-- registerElemEventListener2 l f = withDebug ("Registering layer " <> show l) $ modify_ $ layers . prototypes . at l ?~ f'
--     where f' = flip fmap f $ \df -> (fmap . fmap . fmap) (\(DynSubPass p) -> withDebugBy ("Pass [" <> show (df ^. Pass.desc . Pass.passRep) <> "]") "Running" (DynSubPass p)) df
-- {-# INLINE registerElemEventListener2 #-}

-- FIXME[WD]: pass manager should track pass deps so priority should be obsolete in the future!
attachLayer :: MonadPassManager m => Int -> LayerRep -> ElemRep -> m ()
attachLayer priority l e = withDebug ("Attaching layer " <> show l <> " to (" <> show e <> ")") $ do
    IR.unsafeCreateNewLayer l e
    s <- get
    let Just layerDecls = Map.elems <$> s ^. listeners . decls . at l
        dpasses = catMaybes $ specializeListener e <$> layerDecls
        -- s' = s & layers . attached . at e . non Map.empty . at l ?~ (dpass ^. Pass.desc . Pass.passRep)
    -- put s'
    mapM_ (\(Event.Tagged (Tag es) p) -> addEventListener_byPri priority (Tag $ es <> [switchTypeDesc e]) p) dpasses
    -- TODO: register new available pass!
{-# INLINE attachLayer #-}

queryListeners :: MonadPassManager m => Event.Tag -> m [Uninitialized (GetRefHandler m) (Template (DynPass (GetRefHandler m)))]
queryListeners t = fmap (view Pass.content) . Event.queryListeners t . view (listeners . hub) <$> get ; {-# INLINE queryListeners #-}


addEventListener_byPri :: MonadPassManager m => Int -> Tag -> Pass.Describbed (Uninitialized (GetRefHandler m) (Template (DynPass (GetRefHandler m)))) -> m ()
addEventListener_byPri priority tag p = withDebug ("Attaching event listener " <> show rep <> " to " <> show tag)
                                      $ modify_ $ (listeners . hub %~ attachListener (Listener tag $ SortedListenerRep priority $ switchTypeDesc rep) p)
    where rep = p ^. Pass.desc . Pass.passRep
{-# INLINE addEventListener_byPri #-}


addEventListenerDyn :: MonadPassManager m => Tag -> Pass.Describbed (Uninitialized (GetRefHandler m) (Template (DynPass (GetRefHandler m)))) -> m ()
addEventListenerDyn = addEventListener_byPri 100 ; {-# INLINE addEventListenerDyn #-}

addEventListener :: forall tag m. (Event.KnownTag tag, MonadPassManager m) => Pass.Describbed (Uninitialized (GetRefHandler m) (Template (DynPass (GetRefHandler m)))) -> m ()
addEventListener = addEventListenerDyn (Event.fromPath @tag) ; {-# INLINE addEventListener #-}


-- === Instances === --

-- Logging
instance MonadLogging m => MonadLogging (PassManager  m)

-- Primitive
instance PrimMonad m => PrimMonad (PassManager m) where
    type PrimState (PassManager m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}

instance MonadTrans PassManager where
    lift = wrap' . lift ; {-# INLINE lift #-}




type instance RefData Event _ m = Template (DynPass m)


instance (MonadPassManager m, Pass.ContainsRef pass Event e (GetRefHandler m))
      => Emitter e (SubPass pass m) where
    emit (Payload p) = do
        tmpl <- unwrap' . view (Pass.findRef @pass @Event @e) <$> Pass.get
        liftRefHandler $ Pass.runDynPass $ Pass.unsafeInstantiate p tmpl


setAttr :: MonadPassManager m => AttrRep -> a -> m ()
setAttr = unsafeWriteAttr

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
instance MonadIR m => MonadRef (PassManager m) where
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


instance MonadLogging m => MonadLogging (RefCache m)

instance (MonadIR m, MonadRefCache m) => MonadRefLookup Attr (PassManager m) where
    uncheckedLookupRef a = fmap unsafeCoerce . (^? (attrs . ix (fromTypeDesc a))) <$> get ; {-# INLINE uncheckedLookupRef #-}

instance MonadIR m => MonadRefLookup Layer (PassManager m) where
    uncheckedLookupRef t = do
        ir <- getIR
        let (_,[e,l]) = splitTyConApp t -- dirty typrep of (e // l) extraction
            mlv = ir ^? wrapped' . ix (fromTypeDesc e)
        mr <- liftRefHandler $ mapM (Store.readKey (fromTypeDesc l)) mlv
        return $ wrap' <$> join mr
    {-# INLINE uncheckedLookupRef #-}

instance MonadIR m => MonadRefLookup Net (PassManager m) where
    uncheckedLookupRef a = fmap wrap' . (^? (wrapped' . ix (fromTypeDesc a))) <$> liftRefHandler getIR ; {-# INLINE uncheckedLookupRef #-}


instance (MonadIR m, MonadRefCache m) => MonadRefLookup Event (PassManager m) where
    uncheckedLookupRef a = do
        let ckey = (getTypeDesc @Event, a)
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

--
--
-- -----------------------
-- -- === BuildPlan === --
-- -----------------------
--
-- newtype BuildPlan = BuildPlan [BuildStep]
--
-- data BuildStep = PassEvel
