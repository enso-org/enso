module OCI.Pass.Scheduler where

import Prologue as P

import qualified Control.Monad.State.Layered as State
import qualified OCI.Pass.Definition         as Pass
import qualified OCI.Pass.Dynamic            as Dynamic
import qualified OCI.Pass.Encoder            as Encoder
import qualified OCI.Pass.Registry           as Registry

import Control.Monad.State.Layered (MonadState, StateT)
import Data.Map.Strict             (Map)
import GHC.Exts                    (Any)
import OCI.Pass.Definition         (Pass)
import OCI.Pass.Dynamic            (DynamicPass)



-------------------
-- === State === --
-------------------

-- === Definition === --

data State = State
    { _passes        :: !(Map SomeTypeRep DynamicPass)
    , _attrs         :: !(Map SomeTypeRep Any)
    , _encoderConfig :: !Encoder.State
    }


-- === API === --

buildState :: Encoder.State -> State
buildState = State mempty mempty ; {-# INLINE buildState #-}



----------------------
-- === Registry === --
----------------------

-- === Definition === --

type Monad m = MonadRegistry m
type MonadRegistry m = (MonadState State m, MonadIO m)

newtype SchedulerT m a = SchedulerT (StateT State m a)
    deriving ( Applicative, Alternative, Functor, P.Monad, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadTrans, MonadThrow)
makeLenses ''SchedulerT


-- === Running === --

runT  :: MonadIO m => SchedulerT m a -> Registry.State -> m (a, State)
execT :: MonadIO m => SchedulerT m a -> Registry.State -> m State
runT  f = State.runT (unwrap f) . buildState <=< Encoder.computeConfig ; {-# INLINE runT  #-}
execT   = fmap snd .: runT ; {-# INLINE execT #-}


-- === API === --

-- setAttr
-- registerPass :: Pass pass a -> m ()
-- registerPass =


-- === Instances === --

instance P.Monad m => State.MonadGetter State (SchedulerT m) where
    get = wrap State.get' ; {-# INLINE get #-}

instance P.Monad m => State.MonadSetter State (SchedulerT m) where
    put = wrap . State.put' ; {-# INLINE put #-}
