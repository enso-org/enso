{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableSuperClasses   #-}
{-# LANGUAGE UndecidableInstances      #-}

module OCI.Pass.Manager where

import Luna.Prelude

import           Control.Monad.State.Dependent hiding (State)
import qualified Data.Map            as Map
import           Data.Map            (Map)
import qualified Data.Set            as Set
import           Data.Set            (Set)
import           Data.Event          (Event, Payload(Payload), EventHub, Emitter, Listener(Listener), Tag(Tag), attachListener)
import qualified Data.Event          as Event

import OCI.IR.Class as IR
import OCI.Pass.Class (SubPass, PassRep, Proto, Template, Uninitialized, PassConstruction, DynTmplSubPassM, DynSubPassM)
import qualified OCI.Pass.Class as Pass

import qualified Prologue.Prim as Prim
import Data.TypeDesc

import System.Log
import qualified Data.ManagedVectorMap as Store
import OCI.IR.Layer.Class

import Control.Monad.Raise


data SortedListenerRep = SortedListenerRep Int Event.ListenerRep deriving (Show, Eq)
instance Ord SortedListenerRep where
    compare (SortedListenerRep i r) (SortedListenerRep i' r') = if i == i' then compare r r' else compare i i'



--------------------
-- === Errors === --
--------------------

newtype PassEvalError = PassEvalError SomeException deriving (Show)
makeWrapped ''PassEvalError

instance Exception PassEvalError where
    displayException = displayException . unwrap




-- FIXME[WD]: Refactor or move somewhere else
--            DynPass could not be parametrized - we know the stack, don't we?
--            If not, it should be moved to Pass.hs
--            It is being tested as dynamic source pass loader.
---------------------
-- === DynPass === --
---------------------

type DynTmplSubPass' m a = Pass.Describbed (Uninitialized m (Template (m a)))
type DynTmplPass'    m   = DynTmplSubPass' m ()


type    DynTmplSubPass      = DynTmplSubPassM IO -- FIXME[WD]: refactor to Luna IR dependent code
type    DynSubPass          = DynSubPassM     IO -- FIXME[WD]: refactor to Luna IR dependent code


-------------------
-- === State === --
-------------------


data State m = State { _passes    :: Map PassRep (Pass.Describbed (Uninitialized m (m ()))) -- not used yet
                     , _layers    :: Map ElemRep  $ Map LayerRep PassRep                         -- not used yet
                     , _listeners :: ListenerState m
                     , _attrs     :: Map AttrRep Prim.AnyData
                     } deriving (Show)

data ListenerState m = ListenerState { _decls :: Map LayerRep $ Map PassRep $ ListenerDecl m
                                     , _hub   :: EventHub SortedListenerRep (DynTmplPass' m)
                                     } deriving (Show)

data ListenerDecl m = GenericListener  (Proto $ Event.Tagged (DynTmplPass' m))
                    | SpecificListener ElemRep (Event.Tagged (DynTmplPass' m))
                    deriving (Show)

type RefState m = State (GetRefHandler m)

makeLenses ''State
makeLenses ''ListenerState


-- === Utils === --

specializeListener :: ElemRep -> ListenerDecl m -> Maybe (Event.Tagged (DynTmplPass' m))
specializeListener e = \case
    GenericListener  p    -> Just $ Pass.specialize p e
    SpecificListener le p -> guarded (le == e) p


-- === instances === --

instance Default (ListenerState m) where def = ListenerState def def
instance Default (State         m) where def = State         def def def def



--------------------------
-- === Pass Manager === --
--------------------------

-- === Definition === --

newtype PassManager m a = PassManager (StateT (State (PassManager m)) m a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadLogging, MonadThrow)
makeWrapped ''PassManager


-- === MonadPassManager === --

-- FIXME[WD]: simplify the constraints vvv
type MonadPassRunner     m = (MonadPassManager m, Throws PassEvalError m)
type MonadPassManager    m = (MonadPassManager' m, MonadPassManager' (GetRefHandler m), MonadState (RefState m) m
                             , MonadState Cache m, MonadState Cache (GetRefHandler m)) -- FIXME[WD]: these constraints are added because we need to evaluate nested irs. We should think if we really need it and if we need it here
type MonadPassManager'   m = (MonadPassManagerCtx m, MonadState (RefState m) m)
type MonadPassManagerCtx m = (MonadIR m, MonadRef m, PassConstruction m, MonadRefStore Attr m)


-- === Running === --

evalPassManager :: Logging m => PassManager m a -> State (PassManager m) -> m a
evalPassManager = withDebugBy "PassManager" "Running PassManager" .: evalStateT . unwrap'

evalPassManager' :: Logging m => PassManager m a -> m a
evalPassManager' = flip evalPassManager def


-- === Utils === --

registerElemEventListener :: MonadPassManager m => LayerRep -> PassRep -> ListenerDecl (GetRefHandler m) -> m ()
registerElemEventListener l p f = modify_ @State $ listeners . decls . at l . subMapAt p ?~ f
    -- where f' = flip (fmap . fmap) f $ \df -> (fmap . fmap . fmap) (withDebugBy ("Pass [" <> show (df ^. Pass.description . Pass.passRep) <> "]") "Running") df

registerGenericElemEventListener :: MonadPassManager m => LayerRep -> PassRep -> Proto (Event.Tagged (Pass.Describbed (Uninitialized (GetRefHandler m) (Template (GetRefHandler m ()))))) -> m ()
registerGenericElemEventListener l p f = withDebug (convert $ "Registering event listener " <> show p <> " (* // " <> show l <> ")") $ registerElemEventListener l p (GenericListener f')
    where f' = flip (fmap . fmap) f $ \df -> (fmap . fmap . fmap) (withDebugBy (convert $ "Pass [" <> show (df ^. Pass.description . Pass.passRep) <> "]") "Running") df

registerSpecificElemEventListener :: MonadPassManager m => ElemRep -> LayerRep -> PassRep -> Event.Tagged (Pass.Describbed (Uninitialized (GetRefHandler m) (Template (GetRefHandler m ())))) -> m ()
registerSpecificElemEventListener e l p f = withDebug (convert $ "Registering event listener " <> show p <> " (" <> show e <> " // " <> show l <> ")") $ registerElemEventListener l p (SpecificListener e f')
    where f' = flip fmap f $ \df -> (fmap . fmap . fmap) (withDebugBy (convert $ "Pass [" <> show (df ^. Pass.description . Pass.passRep) <> "]") "Running") df

-- registerElemEventListener' :: MonadPassManager m => LayerRep -> PassRep -> Proto (Event.Tagged (Pass.Describbed (Uninitialized (GetRefHandler m) (Template (Delayed_ (GetRefHandler m)))))) -> m ()
-- registerElemEventListener' = registerElemEventListener Nothing

-- registerElemEventListener2 :: MonadPassManager m => LayerRep -> Proto (Pass.Describbed (Uninitialized (GetRefHandler m) (Template (Delayed_ (GetRefHandler m))))) -> m ()
-- registerElemEventListener2 l f = withDebug ("Registering layer " <> show l) $ modify_ $ layers . prototypes . at l ?~ f'
--     where f' = flip fmap f $ \df -> (fmap . fmap . fmap) (\(Delayed p) -> withDebugBy ("Pass [" <> show (df ^. Pass.description . Pass.passRep) <> "]") "Running" (Delayed p)) df

-- FIXME[WD]: pass manager should track pass deps so priority should be obsolete in the future!
attachLayer :: (MonadPassManager m, Throws IRError m) => Int -> LayerRep -> ElemRep -> m ()
attachLayer priority l e = withDebug (convert $ "Attaching layer " <> show l <> " to (" <> show e <> ")") $ do
    IR.unsafeCreateNewLayer l e
    s <- get @State
    let Just layerDecls = Map.elems <$> s ^. listeners . decls . at l
        dpasses = catMaybes $ specializeListener e <$> layerDecls
        -- s' = s & layers . attached . at e . non Map.empty . at l ?~ (dpass ^. Pass.description . Pass.passRep)
    -- put s'
    mapM_ (\(Event.Tagged (Tag es) p) -> addEventListener_byPri priority (Tag $ es <> [switchTypeDesc e]) p) dpasses
    -- TODO: register new available pass!

queryListeners :: MonadPassManager m => Event.Tag -> m [Uninitialized (GetRefHandler m) (Template (GetRefHandler m ()))]
queryListeners t = fmap copoint . Event.queryListeners t . view (listeners . hub) <$> get @State


addEventListener_byPri :: MonadPassManager m => Int -> Tag -> Pass.Describbed (Uninitialized (GetRefHandler m) (Template (GetRefHandler m ()))) -> m ()
addEventListener_byPri priority tag p = withDebug (convert $ "Attaching event listener " <> show rep <> " to " <> show tag)
                                      $ modify_ @State $ (listeners . hub %~ attachListener (Listener tag $ SortedListenerRep priority $ switchTypeDesc rep) p)
    where rep = p ^. Pass.description . Pass.passRep


addEventListenerDyn :: MonadPassManager m => Tag -> Pass.Describbed (Uninitialized (GetRefHandler m) (Template (GetRefHandler m ()))) -> m ()
addEventListenerDyn = addEventListener_byPri 100

addEventListener :: forall tag m. (Event.KnownTag tag, MonadPassManager m) => Pass.Describbed (Uninitialized (GetRefHandler m) (Template (GetRefHandler m ()))) -> m ()
addEventListener = addEventListenerDyn (Event.fromPath @tag)


-- === Instances === --

instance s ~ RefState m => InferState State m s

instance {-# OVERLAPPABLE #-} MonadGetter s m => MonadGetter s (PassManager m)
instance {-# OVERLAPPABLE #-} MonadSetter s m => MonadSetter s (PassManager m)
instance (m' ~ PassManager m, MonadPassManagerCtx m', Monad m) => MonadGetter (State m') (PassManager m) where get' = wrap'   _get
instance (m' ~ PassManager m, MonadPassManagerCtx m', Monad m) => MonadSetter (State m') (PassManager m) where put' = wrap' . _put


-- Primitive
instance PrimMonad m => PrimMonad (PassManager m) where
    type PrimState (PassManager m) = PrimState m
    primitive = lift . primitive

instance MonadTrans PassManager where
    lift = wrap' . lift




type instance RefData Event _ m = Template (m ())


instance (MonadPassManager m, Pass.ContainsRef pass Event e (GetRefHandler m))
      => Emitter e (SubPass pass m) where
    emit (Payload p) = do
        tmpl <- unwrap' . view (Pass.findRef @pass @Event @e) <$> Pass.get
        liftRefHandler $ Pass.unsafeInstantiate p tmpl


setAttr :: MonadPassManager m => AttrRep -> a -> m ()
setAttr = unsafePutAttr

unsafePutAttr :: MonadPassManager m => AttrRep -> a -> m ()
unsafePutAttr r a = modify_ @State $ attrs %~ Map.insert r (unsafeCoerce a)

unsafeGetAttr :: MonadPassManager m => AttrRep -> m (Maybe Prim.AnyData)
unsafeGetAttr r = view (attrs . at r) <$> get @State

unsafeSetAttr :: MonadPassManager m => AttrRep -> Maybe Prim.AnyData -> m ()
unsafeSetAttr r a = modify_ @State $ attrs . at r .~ a

unsafeDeleteAttr :: MonadPassManager m => AttrRep -> m ()
unsafeDeleteAttr r = modify_ @State $ attrs %~ Map.delete r

unsafeWithAttr :: MonadPassManager m => AttrRep -> a -> m t -> m t
unsafeWithAttr r a f = do
    oldAttr <- unsafeGetAttr r
    unsafePutAttr r a
    out <- f
    unsafeSetAttr r oldAttr
    return out




type instance GetRefHandler (PassManager m) = PassManager m
instance MonadIR m => MonadRef (PassManager m) where
    liftRefHandler = id




----------------------
-- === RefCache === --
----------------------

type Cache = Map (TypeDesc,TypeDesc) Prim.Any


-- FIXME: Refactor errors vvv
data RefLookupError = RefLookupError TypeDesc TypeDesc deriving (Show)
instance Exception RefLookupError


-- === Instances === --

instance (MonadIR m, MonadState Cache m, Throws RefLookupError m) => MonadRefLookup Attr (PassManager m) where
    uncheckedLookupRef a = tryJust (RefLookupError (getTypeDesc @Attr) a) . fmap unsafeCoerce . (^? (attrs . ix (fromTypeDesc a))) =<< get @State

-- TODO[MK]: Possibly MonadRefStore and MonadRefLookup should be merged, but for now only one instance of the former is needed.
instance (MonadIR m, Throws RefLookupError m, MonadState Cache m) => MonadRefStore Attr (PassManager m) where
    uncheckedStoreRef a d = modify_ @State $ attrs . at (fromTypeDesc a) ?~ unsafeCoerce d

instance (MonadIR m, Throws RefLookupError m) => MonadRefLookup Layer (PassManager m) where
    uncheckedLookupRef t = do
        ir <- get @IRST
        let (_,[e,l]) = splitTyConApp t -- dirty typrep of (e // l) extraction
            mlv = ir ^? ix (fromTypeDesc e)
        mr <- liftRefHandler $ mapM (Store.readKey (fromTypeDesc l)) mlv
        wrap' <$> tryJust (RefLookupError (getTypeDesc @Layer) t) (join mr)

instance (MonadIR m, Throws RefLookupError m) => MonadRefLookup Net (PassManager m) where
    uncheckedLookupRef a = tryJust (RefLookupError (getTypeDesc @Net) a) . fmap wrap' . (^? (ix (fromTypeDesc a))) =<< liftRefHandler (get @IRST)


instance (MonadIR m, MonadState Cache m, Throws RefLookupError m) => MonadRefLookup Event (PassManager m) where
    uncheckedLookupRef a = do
        let ckey = (getTypeDesc @Event, a)
        c <- get @Cache
        case c ^. at ckey of
            Just v  -> do
                return $ unsafeCoerce v
            Nothing -> mdo
                put @Cache $ c & at ckey .~ Just (unsafeCoerce out)
                out <- fmap (Ref . fmap sequence_) . fmap sequence . sequence . fmap Pass.initialize =<< queryListeners (Event.fromPathDyn a)
                return out


withFreshIR :: (MonadPassManager m, MonadIR m, MonadState Cache m) => m a -> m a
withFreshIR m = do
    ir    <- get @IRST
    newIR <- wrap <$> mapM Store.emptyFromStructure (unwrap ir)
    with @Cache def $ with @IRST newIR m

evalWithFreshIR :: forall pass m a. (MonadRef m, Pass.PassInit pass m, MonadPassManager m, MonadState Cache m) => SubPass pass m a -> m a
evalWithFreshIR pass = withFreshIR $ Pass.eval' pass


-- -----------------------
-- -- === BuildPlan === --
-- -----------------------
--
-- newtype BuildPlan = BuildPlan [BuildStep]
--
-- data BuildStep = PassEvel
