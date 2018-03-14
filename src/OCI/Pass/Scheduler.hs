module OCI.Pass.Scheduler where

import Prologue as P

import qualified Control.Concurrent.Async    as Async
import qualified Control.Monad.State.Layered as State
import qualified Data.Map.Strict             as Map
import qualified OCI.Pass.Attr               as Attr
import qualified OCI.Pass.Definition         as Pass
import qualified OCI.Pass.Dynamic            as Pass.Dynamic
import qualified OCI.Pass.Encoder            as Encoder
import qualified OCI.Pass.Registry           as Registry

import Control.Concurrent.Async    (Async, async)
import Control.Monad.State.Layered (MonadState, StateT)
import Data.Map.Strict             (Map)
import GHC.Exts                    (Any)
import OCI.Pass.Definition         (Pass)
import OCI.Pass.Dynamic            (DynamicPass)

type M = P.Monad


data DynAttr = DynAttr
    { _defVal :: Any
    }
makeLenses ''DynAttr


-------------------
-- === State === --
-------------------

-- === Definition === --

data State = State
    { _passes   :: !(Map Pass.Rep DynamicPass)
    , _attrDefs :: !(Map Attr.Rep DynAttr)
    , _attrs    :: !(Map Attr.Rep Any)
    , _layout   :: !Encoder.State
    }
makeLenses ''State


-- === API === --

buildState :: Encoder.State -> State
buildState = State mempty mempty mempty ; {-# INLINE buildState #-}



----------------------
-- === Registry === --
----------------------

-- === Definition === --

type Monad m = MonadScheduler m
type MonadScheduler m = (MonadState State m, MonadIO m)

newtype SchedulerT m a = SchedulerT (StateT State m a)
    deriving ( Applicative, Alternative, Functor, M, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadTrans, MonadThrow)
makeLenses ''SchedulerT


-- === Running === --

runT  :: MonadIO m => SchedulerT m a -> Registry.State -> m (a, State)
execT :: MonadIO m => SchedulerT m a -> Registry.State -> m State
runT  f = State.runT (unwrap f) . buildState <=< Encoder.computeConfig ; {-# INLINE runT  #-}
execT   = fmap snd .: runT ; {-# INLINE execT #-}


-- === Passes === --

registerPass :: ∀ pass m.
    ( Typeable             pass
    , Pass.Definition      pass
    , Pass.Dynamic.Compile pass m
    , MonadScheduler            m
    ) => m ()
registerPass = do
    lyt     <- view layout <$> State.get @State
    dynPass <- Pass.Dynamic.compile (Pass.definition @pass) lyt
    State.modify_ @State $ passes . at (Pass.rep @pass) .~ Just dynPass
{-# INLINE registerPass #-}


-- === Attrs === --

registerAttr :: ∀ a m. (Default a, MonadScheduler m, Typeable a) => m ()
registerAttr = State.modify_ @State
             $ attrDefs . at (Attr.rep @a) .~ Just da
    where da = DynAttr . unsafeCoerce $ def @a
{-# INLINE registerAttr #-}

enableAttr :: MonadScheduler m => Attr.Rep -> m ()
enableAttr rep = State.modify_ @State $ \s ->
    let Just dynAttr = Map.lookup rep (s ^. attrDefs) -- FIXME[WD]
    in  s & attrs . at rep .~ Just (dynAttr ^. defVal)
{-# INLINE enableAttr #-}

disableAttr :: MonadScheduler m => Attr.Rep -> m ()
disableAttr rep = State.modify_ @State $ attrs . at rep .~ Nothing
{-# INLINE disableAttr #-}

enableAttrByType  :: ∀ attr m. (MonadScheduler m, Typeable attr) => m ()
disableAttrByType :: ∀ attr m. (MonadScheduler m, Typeable attr) => m ()
enableAttrByType  = enableAttr  $ Attr.rep @attr ; {-# INLINE enableAttrByType #-}
disableAttrByType = disableAttr $ Attr.rep @attr ; {-# INLINE disableAttrByType #-}


-- === Instances === --

instance P.Monad m => State.MonadGetter State (SchedulerT m) where
    get = wrap State.get' ; {-# INLINE get #-}

instance P.Monad m => State.MonadSetter State (SchedulerT m) where
    put = wrap . State.put' ; {-# INLINE put #-}



------------------------
-- === PassWorker === --
------------------------

-- === Defintion === --

newtype PassWorker = PassWorker (Async ())
makeLenses ''PassWorker


-- === API === --

runPass :: MonadScheduler m => Pass.Rep -> m PassWorker
runPass passRep = do
    state <- State.get @State
    let Just dynPass = Map.lookup passRep $ state ^. passes -- FIXME[WD]
    liftIO $ fmap wrap . async . Pass.Dynamic.run dynPass $ (wrap $ state ^. attrs) -- FIXME: wrap on AttrMap is ugly
{-# INLINE runPass #-}

runPassByType :: ∀ pass m. (MonadScheduler m, Typeable pass) => m PassWorker
runPassByType = runPass $ Pass.rep @pass ; {-# INLINE runPassByType #-}

wait :: MonadIO m => PassWorker -> m ()
wait = liftIO . Async.wait . unwrap ; {-# INLINE wait #-}
