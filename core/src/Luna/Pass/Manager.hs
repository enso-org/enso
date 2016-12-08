module Luna.Pass.Manager where

import Luna.Prelude

import qualified Control.Monad.State as State
import           Control.Monad.State (StateT, evalStateT, runStateT)
import qualified Data.Map            as Map
import           Data.Map            (Map)
import           Data.Event          (ListenerHub)
import qualified Data.Event          as Event

import Luna.IR.Internal.IR
import Luna.Pass.Class (DynPass)



-------------------
-- === State === --
-------------------

data State m = State { _layerCons   :: Map LayerRep (PMPass m)
                     , _listenerHub :: ListenerHub  (PMPass m)
                     }

type PMPass m = DynPass (PassManager m)


-- === instances === --

instance Default (State m) where
    def = State def def ; {-# INLINE def #-}


--------------------------
-- === Pass Manager === --
--------------------------

-- === Definition === --

newtype PassManager m a = PassManager (StateT (State m) m a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)
makeWrapped ''PassManager

type GetMonad m = PassManager (GetMonadBase m)

type family GetMonadBase m where
    GetMonadBase (PassManager m) = m
    GetMonadBase (t m)           = GetMonadBase m

type PMPass' m = PMPass (GetMonadBase m)
type State' m = State (GetMonadBase m)

class Monad m => MonadPassManager m where
    get :: m (State' m)
    put :: State' m -> m ()

instance Monad m => MonadPassManager (PassManager m) where
    get = wrap'   State.get ; {-# INLINE get #-}
    put = wrap' . State.put ; {-# INLINE put #-}


-- === Running === --

evalPassManager :: Monad m => PassManager m a -> State m -> m a
evalPassManager = evalStateT . unwrap' ; {-# INLINE evalPassManager #-}

evalPassManager' :: Monad m => PassManager m a -> m a
evalPassManager' = flip evalPassManager def ; {-# INLINE evalPassManager' #-}


-- === Utils === --

makeLenses ''State -- FIXME [WD]: refactor

queryListeners :: MonadPassManager m => Event.Tag -> m [Event.Listener (PMPass' m)]
queryListeners t = Event.queryListeners t . view listenerHub <$> get




-- === Instances === --

-- Primitive
instance PrimMonad m => PrimMonad (PassManager m) where
    type PrimState (PassManager m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}

instance MonadTrans PassManager where
    lift = wrap' . lift ; {-# INLINE lift #-}
