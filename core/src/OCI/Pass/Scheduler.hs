module OCI.Pass.Scheduler where

import Prologue as P

import qualified Control.Concurrent.Async    as Async
import qualified Control.Monad.Exception     as Exception
import qualified Control.Monad.State.Layered as State
import qualified Data.List                   as List
import qualified Data.Map.Strict             as Map
import qualified OCI.Pass.Attr               as Attr
import qualified OCI.Pass.Class         as Pass
import qualified OCI.Pass.Dynamic            as Pass
import qualified OCI.Pass.State.Encoder            as Encoder
import qualified OCI.Pass.State.IRInfo               as Info
import qualified OCI.Pass.Registry           as Registry

import Control.Concurrent.Async    (Async, async)
import Control.Monad.Exception     (Throws, throw)
import Control.Monad.State.Layered (StateT)
import Data.Map.Strict             (Map)
import GHC.Exts                    (Any)
import OCI.Pass.Class         (Pass)
import OCI.Pass.Dynamic            (DynamicPass)
import OCI.Pass.Registry           (RegistryT)

type M = P.Monad


--------------------
-- === Errors === --
--------------------

data Error
    = MissingPass  Pass.Rep
    | MissingAttrs [Attr.Rep]
    deriving (Show)

instance Exception Error



-------------------
-- === State === --
-------------------

-- === Definition === --

data State = State
    { _passes   :: !(Map Pass.Rep DynamicPass)
    , _attrDefs :: !(Map Attr.Rep DynAttr)
    , _attrs    :: !(Map Attr.Rep Any)
    , _layout   :: !Info.CompiledInfo
    }

data DynAttr = DynAttr
    { _defVal :: Any
    , _fanIn  :: NonEmpty Any -> IO Any
    }

makeLenses ''State
makeLenses ''DynAttr


-- === API === --

buildState :: Info.CompiledInfo -> State
buildState = State mempty mempty mempty ; {-# INLINE buildState #-}



----------------------
-- === Registry === --
----------------------

-- === Definition === --

type Monad m = MonadScheduler m
type MonadScheduler m =
    ( State.Monad State m
    , MonadIO m
    , Throws Error m
    , Throws Encoder.Error m
    )

newtype SchedulerT m a = SchedulerT (StateT State m a)
    deriving ( Applicative, Alternative, Functor, M, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadTrans, MonadThrow)
makeLenses ''SchedulerT


-- === Running === --

runT  :: MonadIO m => SchedulerT m a -> Info.Info -> m (a, State)
execT :: MonadIO m => SchedulerT m a -> Info.Info -> m State
evalT :: MonadIO m => SchedulerT m a -> Info.Info -> m a
runT  f = State.runT (unwrap f) . buildState <=< Info.compile ; {-# INLINE runT  #-}
execT   = fmap snd .: runT ; {-# INLINE execT #-}
evalT   = fmap fst .: runT ; {-# INLINE evalT #-}

runManual :: MonadIO m => RegistryT m () -> SchedulerT m a -> m a
runManual freg fsched = do
    reg <- Registry.execT freg
    evalT fsched reg
{-# INLINE runManual #-}


-- === Passes === --

type PassRegister pass m =
    ( Typeable       pass
    , Pass.Compile   pass m
    , MonadScheduler m
    )

registerPass :: ∀ pass m. (PassRegister pass m, Pass.Definition pass) => m ()
registerPass = registerPassFromFunction__ (Pass.definition @pass) ; {-# INLINE registerPass #-}

registerPassFromFunction__ :: ∀ pass m.
    PassRegister pass m => Pass pass () -> m ()
registerPassFromFunction__ !pass = do
    !lyt     <- view layout <$> State.get @State
    !dynPass <- Pass.compile pass lyt
    State.modify_ @State $ passes . at (Pass.rep @pass) .~ Just dynPass
{-# INLINE registerPassFromFunction__ #-}


-- === Attrs === --

registerAttr :: ∀ a m. (Default a, MonadScheduler m, Typeable a, Attr.FanIn a IO) => m ()
registerAttr = State.modify_ @State
             $ attrDefs . at (Attr.rep @a) .~ Just da
    where da = DynAttr (unsafeCoerce $ def @a)
             $ unsafeCoerce $ Attr.fanIn @a @IO
{-# INLINE registerAttr #-}

enableAttr :: MonadScheduler m => Attr.Rep -> m ()
enableAttr rep = State.modifyM_ @State $ \s -> do
    dynAttr <- Exception.fromJust (MissingAttrs [rep])
             $ Map.lookup rep (s ^. attrDefs)
    pure     $ s & attrs . at rep .~ Just (dynAttr ^. defVal)
{-# INLINE enableAttr #-}

disableAttr :: MonadScheduler m => Attr.Rep -> m ()
disableAttr rep = State.modify_ @State $ attrs . at rep .~ Nothing
{-# INLINE disableAttr #-}

enableAttrByType  :: ∀ attr m. (MonadScheduler m, Typeable attr) => m ()
disableAttrByType :: ∀ attr m. (MonadScheduler m, Typeable attr) => m ()
enableAttrByType  = enableAttr  $ Attr.rep @attr ; {-# INLINE enableAttrByType #-}
disableAttrByType = disableAttr $ Attr.rep @attr ; {-# INLINE disableAttrByType #-}

lookupAttr :: ∀ attr m. (MonadScheduler m, Typeable attr)
           => m (Maybe attr)
lookupAttr = fmap unsafeCoerce . Map.lookup (Attr.rep @attr) . view attrs
         <$> State.get @State
{-# INLINE lookupAttr #-}

setAttr :: ∀ attr m. (MonadScheduler m, Typeable attr) => attr -> m ()
setAttr attr = State.modify_ @State mod where
    mod = attrs %~ Map.insert (Attr.rep @attr) (unsafeCoerce attr)
{-# INLINE setAttr #-}


-- === Instances === --

instance P.Monad m => State.Getter State (SchedulerT m) where
    get = wrap State.get' ; {-# INLINE get #-}

instance P.Monad m => State.Setter State (SchedulerT m) where
    put = wrap . State.put' ; {-# INLINE put #-}



------------------------
-- === PassThread === --
------------------------

-- === Defintion === --

newtype PassThread = PassThread (Async Pass.AttrVals)
makeLenses ''PassThread





-- === Attrib gather === --

waitAndGetAttrs :: MonadIO m => PassThread -> m Pass.AttrVals
waitAndGetAttrs = liftIO . Async.wait . unwrap ; {-# INLINE waitAndGetAttrs #-}

gatherSingle :: MonadScheduler m => Pass.Rep -> PassThread -> m ()
gatherSingle !pass = gather pass . pure ; {-# INLINE gatherSingle #-}

gather :: MonadScheduler m => Pass.Rep -> NonEmpty PassThread -> m ()
gather !passRep !threads = gatherAttrs passRep =<< mapM waitAndGetAttrs threads ; {-# INLINE gather #-}

gatherAttrs :: MonadScheduler m => Pass.Rep -> NonEmpty Pass.AttrVals -> m ()
gatherAttrs !passRep !resultAttrs = do
    !state   <- State.get @State
    !dynPass <- Exception.fromJust (MissingPass passRep)
              $ Map.lookup passRep $ state ^. passes
    let !grpAttrs = transposeList $ unwrap <$> resultAttrs
        !outAttrs = dynPass ^. (Pass.desc . Pass.outputs . Pass.attrs)
        !zippers  = view fanIn <$> dynAttrs
        (!errs, !dynAttrs) = partitionEithers
                           $ flip lookupEither (state ^. attrDefs) <$> outAttrs
    when_ (not $ null errs) . throw $ MissingAttrs errs
    !newAttrs   <- liftIO $ zipWithM ($) zippers grpAttrs
    let !attrs'  = foldl' (flip $ uncurry Map.insert) (state ^. attrs)
                 $ zip outAttrs newAttrs
        !state'  = state & attrs .~ attrs'
    State.put @State state'
{-# INLINE gatherAttrs #-}


-- === Pass Threads === --

initPass :: MonadScheduler m => Pass.Rep -> m (IO Pass.AttrVals)
initPass !passRep = do
    !state   <- State.get @State
    !dynPass <- Exception.fromJust (MissingPass passRep)
              $ Map.lookup passRep $ state ^. passes
    let !attrReps          = dynPass ^. (Pass.desc . Pass.attrLayout)
        (!errs, !attrVals) = partitionEithers
                           $ flip lookupEither (state ^. attrs) <$> attrReps
    when_ (not $ null errs) . throw $ MissingAttrs errs
    pure . Pass.run dynPass $ wrap attrVals
{-# INLINE initPass #-}

forkPass :: MonadScheduler m => Pass.Rep -> m PassThread
forkPass passRep = liftIO . fmap wrap . async =<< initPass passRep ; {-# INLINE forkPass #-}

forkPassByType :: ∀ pass m. (MonadScheduler m, Typeable pass) => m PassThread
forkPassByType = forkPass $ Pass.rep @pass ; {-# INLINE forkPassByType #-}

runPass :: (MonadScheduler m, Throws Error m) => Pass.Rep -> m ()
runPass !rep = gatherSingle rep =<< forkPass rep ; {-# INLINE runPass #-}

runPassByType :: ∀ pass m. (MonadScheduler m, Typeable pass) => m ()
runPassByType = runPass $ Pass.rep @pass ; {-# INLINE runPassByType #-}


-- === Debug Pass runners === --

runPassSameThread :: MonadScheduler m => Pass.Rep -> m ()
runPassSameThread !rep = do
    !attrs <- join (liftIO <$> initPass rep)
    gatherAttrs rep $ pure attrs
{-# INLINE runPassSameThread #-}

runPassSameThreadByType :: ∀ pass m. (MonadScheduler m, Typeable pass) => m ()
runPassSameThreadByType = runPassSameThread $ Pass.rep @pass ; {-# INLINE runPassSameThreadByType #-}

debugRunPassDefs :: ∀ pass m. (Typeable pass, PassRegister pass m)
                 => [Pass pass ()] -> m ()
debugRunPassDefs passes = for_ passes $ \pass -> do
    registerPassFromFunction__ pass
    runPassByType @pass
{-# INLINE debugRunPassDefs #-}

debugRunPassDef :: ∀ pass m. (Typeable pass, PassRegister pass m)
                => Pass pass () -> m ()
debugRunPassDef = debugRunPassDefs . pure ; {-# INLINE debugRunPassDef #-}



-- === Utils === --

lookupEither :: Ord k => k -> Map k v -> Either k v
lookupEither k = note k . Map.lookup k ; {-# INLINE lookupEither #-}

transposeList :: ∀ a. NonEmpty [a] -> [NonEmpty a]
transposeList l = fmap unsafeConvert $ List.transpose (convert l :: [[a]]) ; {-# INLINE transposeList #-}
