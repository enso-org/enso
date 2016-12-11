{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Manager where

import Luna.Prelude

import qualified Control.Monad.State as State
import           Control.Monad.State (StateT, evalStateT, runStateT)
import qualified Data.Map            as Map
import           Data.Map            (Map)
import           Data.Event          (Event, EventHub, Emitter, IsTag, Listener(Listener), toTag, attachListener)
import qualified Data.Event          as Event

import Luna.IR.Internal.IR
import Luna.Pass.Class (IsPass, DynSubPass, SubPass)
import qualified Luna.Pass.Class as Pass

import qualified Prologue.Prim as Prim






-- | Constructor functions ::  t        -> Definition t -> m (LayerData UID t)
-- newtype ConsFunc m = ConsFunc (Prim.Any -> Prim.Any -> PMSubPass m Prim.Any)
-- newtype ConsFunc m = ConsFunc (Prim.Any -> Prim.Any -> PMPass m)
-- makeWrapped ''ConsFunc

-------------------
-- === State === --
-------------------

data State m = State { _eventHub  :: EventHub     (PMPass   m)
                     , _layerCons :: Map LayerRep (PMPass   m)
                     , _attrs     :: Map AttrRep  Prim.AnyData
                     } deriving (Show)

type PMSubPass m = DynSubPass (PassManager m)
type PMPass    m = PMSubPass m ()


-- === instances === --

instance Default (State m) where
    def = State def def def ; {-# INLINE def #-}


--------------------------
-- === Pass Manager === --
--------------------------

-- === Definition === --

newtype PassManager m a = PassManager (StateT (State m) m a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)
makeWrapped ''PassManager

type PassManager' m = PassManager (GetBaseMonad m)

type family GetBaseMonad m where
    GetBaseMonad (PassManager m) = m
    GetBaseMonad (t m)           = GetBaseMonad m

type PMPass' m = PMPass (GetBaseMonad m)
type State' m = State (GetBaseMonad m)

class Monad m => MonadPassManager m where
    get :: m (State' m)
    put :: State' m -> m ()
    liftPassManager :: PassManager' m a -> m a

instance Monad m => MonadPassManager (PassManager m) where
    get = wrap'   State.get ; {-# INLINE get #-}
    put = wrap' . State.put ; {-# INLINE put #-}
    liftPassManager = id

type TransBaseMonad t m = (GetBaseMonad m ~ GetBaseMonad (t m), Monad m, Monad (t m), MonadTrans t)
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

evalPassManager :: Monad m => PassManager m a -> State m -> m a
evalPassManager = evalStateT . unwrap' ; {-# INLINE evalPassManager #-}

evalPassManager' :: Monad m => PassManager m a -> m a
evalPassManager' = flip evalPassManager def ; {-# INLINE evalPassManager' #-}


-- === Utils === --

makeLenses ''State -- FIXME [WD]: refactor

queryListeners :: MonadPassManager m => Event.Tag -> m [PMPass' m]
queryListeners t = Event.queryListeners t . view eventHub <$> get

addEventListener :: (MonadPassManager m, IsPass (PassManager' m) p, IsTag evnt) => evnt -> p (PassManager' m) a -> m ()
addEventListener evnt p = modify_ $ eventHub %~ attachListener (Listener (toTag evnt) $ switchRep $ cp ^. Pass.repr) cp where
    cp = Pass.commit $ Pass.dropResult p
{-# INLINE addEventListener #-}


-- registerGenericLayer :: l -> (t -> Definition t -> m (LayerData UID t)) -> m ()
-- registerGenericLayer _


-- === Instances === --

-- Primitive
instance PrimMonad m => PrimMonad (PassManager m) where
    type PrimState (PassManager m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}

instance MonadTrans PassManager where
    lift = wrap' . lift ; {-# INLINE lift #-}



type instance KeyData m (Event e) = PassManager' m ()


-- Emitter
instance (MonadPassManager m, Pass.ContainsKey (Event e) (Pass.Keys pass)) => Emitter (SubPass pass m) e where
    emit _ _ = liftPassManager . unwrap' . view (Pass.findKey @(Event e)) =<< Pass.get


instance (MonadPassManager m, Typeable a) => KeyMonad (Attr a) m n where
    uncheckedLookupKey = fmap unsafeCoerce . (^? (attrs . ix (typeRep' @a))) <$> get ; {-# INLINE uncheckedLookupKey #-}
