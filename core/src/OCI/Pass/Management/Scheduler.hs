module OCI.Pass.Management.Scheduler where

import Prologue as P

import qualified Control.Concurrent.Async     as Async
import qualified Control.Monad.Exception      as Exception
import qualified Control.Monad.State.Layered  as State
import qualified Data.Graph.Class             as Graph
import qualified Data.List                    as List
import qualified Data.Map.Strict              as Map
import qualified OCI.Pass.Definition.Class    as Pass
import qualified OCI.Pass.Definition.Dynamic  as Pass
import qualified OCI.Pass.Management.Registry as Registry
import qualified OCI.Pass.State.Attr          as Attr
import qualified OCI.Pass.State.Encoder       as Encoder
import qualified OCI.Pass.State.IRInfo        as IRInfo

import Control.Concurrent.Async     (Async, async)
import Control.Monad.Exception      (Throws, throw)
import Control.Monad.State.Layered  (StateT)
import Data.Graph.Class             (Graph)
import Data.Map.Strict              (Map)
import GHC.Exts                     (Any)
import OCI.Pass.Definition.Class    (Pass)
import OCI.Pass.Definition.Dynamic  (DynamicPass)
import OCI.Pass.Management.Registry (RegistryT)
import OCI.Pass.State.IRInfo        (CompiledIRInfo, IRInfo)

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

data State t = State
    { _passes   :: !(Map Pass.Rep (DynamicPass t))
    , _attrDefs :: !(Map Attr.Rep DynAttr)
    , _attrs    :: !(Map Attr.Rep Any)
    , _layout   :: !CompiledIRInfo
    }

data DynAttr = DynAttr
    { _defVal :: Any
    , _fanIn  :: NonEmpty Any -> IO Any
    }

makeLenses ''State
makeLenses ''DynAttr


-- === API === --

buildState :: CompiledIRInfo -> State t
buildState = State mempty mempty mempty ; {-# INLINE buildState #-}



----------------------
-- === Registry === --
----------------------

-- === Definition === --

type Monad t m = MonadScheduler t m
type MonadScheduler t m =
    ( State.Monad (State t) m
    , MonadIO m
    , Throws Error m
    , Throws Encoder.Error m
    )

newtype SchedulerT t m a = SchedulerT (StateT (State t) m a)
    deriving ( Applicative, Alternative, Functor, M, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadTrans, MonadThrow)
makeLenses ''SchedulerT


-- === Running === --

runT  :: MonadIO m => SchedulerT t m a -> IRInfo -> m (a, State t)
execT :: MonadIO m => SchedulerT t m a -> IRInfo -> m (State t)
evalT :: MonadIO m => SchedulerT t m a -> IRInfo -> m a
runT  f = State.runT (unwrap f) . buildState <=< IRInfo.compile ; {-# INLINE runT  #-}
execT   = fmap snd .: runT ; {-# INLINE execT #-}
evalT   = fmap fst .: runT ; {-# INLINE evalT #-}

runManual :: MonadIO m => RegistryT m () -> SchedulerT t m a -> m a
runManual freg fsched = do
    reg <- Registry.execT freg
    evalT fsched reg
{-# INLINE runManual #-}


-- === Passes === --

type PassRegister t pass m =
    ( Typeable       pass
    , Pass.Compile   t pass m
    , MonadScheduler t m
    )

registerPass :: ∀ t pass m. (PassRegister t pass m, Pass.Definition pass) => m ()
registerPass = registerPassFromFunction__ @t (Pass.definition @pass) ; {-# INLINE registerPass #-}

registerPassFromFunction__ :: ∀ t pass m.
    PassRegister t pass m => Pass pass (Graph t) () -> m ()
registerPassFromFunction__ !pass = do
    !lyt     <- view layout <$> State.get @(State t)
    !dynPass <- Pass.compile pass lyt
    State.modify_ @(State t) $ passes . at (Pass.rep @pass) .~ Just dynPass
{-# INLINE registerPassFromFunction__ #-}


-- === Attrs === --

registerAttr :: ∀ t a m. (Default a, MonadScheduler t m, Typeable a, Attr.FanIn a IO) => m ()
registerAttr = State.modify_ @(State t)
             $ attrDefs . at (Attr.rep @a) .~ Just da
    where da = DynAttr (unsafeCoerce $ def @a)
             $ unsafeCoerce $ Attr.fanIn @a @IO
{-# INLINE registerAttr #-}

enableAttr :: ∀ t m. MonadScheduler t m => Attr.Rep -> m ()
enableAttr rep = State.modifyM_ @(State t) $ \s -> do
    dynAttr <- Exception.fromJust (MissingAttrs [rep])
             $ Map.lookup rep (s ^. attrDefs)
    pure     $ s & attrs . at rep .~ Just (dynAttr ^. defVal)
{-# INLINE enableAttr #-}

disableAttr :: ∀ t m. MonadScheduler t m => Attr.Rep -> m ()
disableAttr rep = State.modify_ @(State t) $ attrs . at rep .~ Nothing
{-# INLINE disableAttr #-}

enableAttrByType  :: ∀ t attr m. (MonadScheduler t m, Typeable attr) => m ()
disableAttrByType :: ∀ t attr m. (MonadScheduler t m, Typeable attr) => m ()
enableAttrByType  = enableAttr  @t $ Attr.rep @attr ; {-# INLINE enableAttrByType #-}
disableAttrByType = disableAttr @t $ Attr.rep @attr ; {-# INLINE disableAttrByType #-}

lookupAttr :: ∀ t attr m. (MonadScheduler t m, Typeable attr)
           => m (Maybe attr)
lookupAttr = fmap unsafeCoerce . Map.lookup (Attr.rep @attr) . view attrs
         <$> State.get @(State t)
{-# INLINE lookupAttr #-}

setAttr :: ∀ t attr m. (MonadScheduler t m, Typeable attr) => attr -> m ()
setAttr attr = State.modify_ @(State t) mod where
    mod = attrs %~ Map.insert (Attr.rep @attr) (unsafeCoerce attr)
{-# INLINE setAttr #-}


-- === Instances === --

instance P.Monad m => State.Getter (State t) (SchedulerT t m) where
    get = wrap State.get' ; {-# INLINE get #-}

instance P.Monad m => State.Setter (State t) (SchedulerT t m) where
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

gatherSingle :: ∀ t m. MonadScheduler t m => Pass.Rep -> PassThread -> m ()
gatherSingle !pass = gather @t pass . pure ; {-# INLINE gatherSingle #-}

gather :: ∀ t m. MonadScheduler t m => Pass.Rep -> NonEmpty PassThread -> m ()
gather !passRep !threads = gatherAttrs @t passRep =<< mapM waitAndGetAttrs threads ; {-# INLINE gather #-}

gatherAttrs :: ∀ t m. MonadScheduler t m => Pass.Rep -> NonEmpty Pass.AttrVals -> m ()
gatherAttrs !passRep !resultAttrs = do
    !state   <- State.get @(State t)
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
    State.put @(State t) state'
{-# INLINE gatherAttrs #-}


-- === Pass Threads === --

initPass :: ∀ t m. MonadScheduler t m => Pass.Rep -> m (IO Pass.AttrVals)
initPass !passRep = do
    !state   <- State.get @(State t)
    !dynPass <- Exception.fromJust (MissingPass passRep)
              $ Map.lookup passRep $ state ^. passes
    let !attrReps          = dynPass ^. (Pass.desc . Pass.attrLayout)
        (!errs, !attrVals) = partitionEithers
                           $ flip lookupEither (state ^. attrs) <$> attrReps
    when_ (not $ null errs) . throw $ MissingAttrs errs
    pure . Pass.run dynPass $ wrap attrVals
{-# INLINE initPass #-}

-- forkPass :: MonadScheduler t m => Pass.Rep -> m PassThread
-- forkPass passRep = liftIO . fmap wrap . async =<< initPass passRep ; {-# INLINE forkPass #-}

-- forkPassByType :: ∀ t pass m. (MonadScheduler t m, Typeable pass) => m PassThread
-- forkPassByType = forkPass $ Pass.rep @pass ; {-# INLINE forkPassByType #-}

-- runPass :: (MonadScheduler t m, Throws Error m) => Pass.Rep -> m ()
-- runPass !rep = gatherSingle rep =<< forkPass rep ; {-# INLINE runPass #-}

-- runPassByType :: ∀ t pass m. (MonadScheduler t m, Typeable pass) => m ()
-- runPassByType = runPass $ Pass.rep @pass ; {-# INLINE runPassByType #-}


-- -- === Debug Pass runners === --

runPassSameThread :: Graph.StateEncoder Graph.Luna IO => Pass.Rep -> SchedulerT Graph.Luna IO ()
runPassSameThread !rep = do
    !attrs <- join (lift <$> initPass rep)
    gatherAttrs @Graph.Luna rep $ pure attrs
{-# INLINE runPassSameThread #-}

runPassSameThreadByType :: ∀ pass. Graph.StateEncoder Graph.Luna IO => (Typeable pass) => SchedulerT Graph.Luna IO ()
runPassSameThreadByType = runPassSameThread $ Pass.rep @pass ; {-# INLINE runPassSameThreadByType #-}

-- debugRunPassDefs :: ∀ t pass m. (Typeable pass, PassRegister t pass m)
--                  => [Pass pass IO ()] -> m ()
-- debugRunPassDefs passes = for_ passes $ \pass -> do
--     registerPassFromFunction__ pass
--     runPassByType @pass
-- {-# INLINE debugRunPassDefs #-}

-- debugRunPassDef :: ∀ t pass m. (Typeable pass, PassRegister t pass m)
--                 => Pass pass IO () -> m ()
-- debugRunPassDef = debugRunPassDefs . pure ; {-# INLINE debugRunPassDef #-}



-- === Utils === --

lookupEither :: Ord k => k -> Map k v -> Either k v
lookupEither k = note k . Map.lookup k ; {-# INLINE lookupEither #-}

transposeList :: ∀ a. NonEmpty [a] -> [NonEmpty a]
transposeList l = fmap unsafeConvert $ List.transpose (convert l :: [[a]]) ; {-# INLINE transposeList #-}
