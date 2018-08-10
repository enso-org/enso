{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.Pass.State.Attr where

import           Prologue hiding (Type, Wrapped, read)
import qualified Prologue as P

import qualified Control.Concurrent.MVar     as MVar
import qualified Control.Monad.State.Layered as State

import Control.Concurrent.MVar (MVar)

type T = P.Type


------------------
-- === Attr === --
------------------

-- === Definition === --

type family Type attr :: T
type family Wrapper t :: T -> T

type    Wrapped attr = Wrapper (Type attr)
newtype Attr    attr = Attr (Wrapped attr attr)
makeLenses ''Attr


-- === Instances === --

deriving instance Show    (Wrapped attr attr) => Show    (Attr attr)
deriving instance Default (Wrapped attr attr) => Default (Attr attr)


-- === Getter / Setter === --

-- | Attributes can be 'read' and 'write' using these smart functions.
--   Default instances are the most common use case, when dealing with
--   immutable wrappers. However, if a mutable wrapper is used, like 'MVar'
--   there is no need to 'write' back the 'MVar' when its value is updated.

type Editor attr m = (Getter attr m, Setter attr m)
class Monad m => Getter attr m where get :: m attr
class Monad m => Setter attr m where put :: attr -> m ()

instance {-# OVERLAPPABLE #-} Monad m => Getter Imp  m where get = impossible
instance {-# OVERLAPPABLE #-} Monad m => Setter Imp  m where put = impossible
instance {-# OVERLAPPABLE #-} Getter attr ImpM where get = impossible
instance {-# OVERLAPPABLE #-} Setter attr ImpM where put = impossible

modifyM  :: ∀ attr m t. Editor attr m => (attr -> m (t, attr)) -> m t
modifyM_ :: ∀ attr m.   Editor attr m => (attr -> m     attr)  -> m ()
modify   :: ∀ attr m t. Editor attr m => (attr ->   (t, attr)) -> m t
modify_  :: ∀ attr m.   Editor attr m => (attr ->       attr)  -> m ()
modifyM_  = modifyM  . (fmap.fmap) ((),) ; {-# INLINE modifyM_ #-}
modify    = modifyM  . fmap pure         ; {-# INLINE modify   #-}
modify_   = modifyM_ . fmap pure         ; {-# INLINE modify_  #-}
modifyM f = do (!t,!a) <- f =<< get
               t <$ put a
{-# INLINE modifyM #-}

branch        :: ∀ attr m a. Editor attr m =>                     m a -> m a
with          :: ∀ attr m a. Editor attr m =>  attr            -> m a -> m a
withModified  :: ∀ attr m a. Editor attr m => (attr ->   attr) -> m a -> m a
withModifiedM :: ∀ attr m a. Editor attr m => (attr -> m attr) -> m a -> m a
with              = withModified  . const          ; {-# INLINE with          #-}
withModified      = withModifiedM . fmap pure      ; {-# INLINE withModified  #-}
withModifiedM f m = branch @attr $ modifyM_ f >> m ; {-# INLINE withModifiedM #-}
branch          m = do s <- get @attr
                       m <* put s
{-#INLINE branch #-}


-- === TypedGetter / TypedSetter === --

class Monad m => TypedGetter t attr m where
    getTyped :: Type attr ~ t => m attr

class Monad m => TypedSetter t attr m where
    putTyped :: Type attr ~ t => attr -> m ()

instance (Monad m, TypedGetter (Type attr) attr m)
      => Getter attr m where get = getTyped ; {-# INLINE get #-}

instance (Monad m, TypedSetter (Type attr) attr m)
      => Setter attr m where put = putTyped ; {-# INLINE put #-}


-- === Fan in / out === --

type FanIn attr = FanInTyped (Type attr) attr
class Monad m => FanInTyped t attr m where
    fanInTyped :: t ~ Type attr => NonEmpty (Attr attr) -> m (Attr attr)

class Monad m => FanOut__ t attr m where
    fanOut     :: t ~ Type attr => m (Attr attr)
    fanOutMany :: t ~ Type attr => Int -> m [Attr attr]
    fanOutMany = flip replicateM fanOut ; {-# INLINE fanOutMany #-}


fanIn :: ∀ attr m. FanIn attr m => NonEmpty (Attr attr) -> m (Attr attr)
fanIn = fanInTyped ; {-# INLINE fanIn #-}



-----------------
-- === Rep === --
-----------------

-- === Definition === --

newtype Rep = Rep SomeTypeRep deriving (Eq, Ord, Show)
makeLenses ''Rep


-- === API === --

rep :: ∀ (attr :: T). Typeable attr => Rep
rep = wrap $ someTypeRep @attr ; {-# INLINE rep #-}

reps :: ∀ (attrs :: [T]). TypeableMany attrs => [Rep]
reps = wrap <$> someTypeReps @attrs ; {-# INLINE reps #-}

repOf :: ∀ attr. Typeable attr => Attr attr -> Rep
repOf _ = rep @attr ; {-# INLINE repOf #-}



--------------------
-- === Atomic === --
--------------------

-- | Atomic attribute allows running passes in parallel if they are only
--   reading the attribute. If at least one pass writes it, it will be run
--   alone.

data Atomic
type instance Wrapper Atomic = Identity

instance (Monad m, State.Getter (Attr attr) m)
      => TypedGetter Atomic attr m where
    getTyped = unwrap . unwrap <$> State.get @(Attr attr) ; {-# INLINE getTyped #-}

instance (Monad m, State.Setter (Attr attr) m)
      => TypedSetter Atomic attr m where
    putTyped = State.put @(Attr attr) . wrap . wrap ; {-# INLINE putTyped #-}

instance Monad m => FanInTyped Atomic attr m where
    fanInTyped = \case
        a :| [] -> pure a
        _ -> error "Impossible happened: Atomic attribute used in parallel."
    {-# INLINE fanInTyped #-}



-----------------------
-- === ParAppend === --
-----------------------

-- | ParAppend attribute is copied to all passes which can be run in parallel
--   and is gathered after they successfully end. The results are concatenated
--   and thus the data have to implement 'Semigroup'.

data ParAppend
type instance Wrapper ParAppend = Identity

instance (Monad m, State.Getter (Attr attr) m)
      => TypedGetter ParAppend attr m where
    getTyped = unwrap . unwrap <$> State.get @(Attr attr) ; {-# INLINE getTyped #-}

instance (Monad m, State.Setter (Attr attr) m)
      => TypedSetter ParAppend attr m where
    putTyped = State.put @(Attr attr) . wrap . wrap ; {-# INLINE putTyped #-}

instance (Monad m, Semigroup attr)
      => FanInTyped ParAppend attr m where
    fanInTyped = pure . wrap . wrap . fold1 . fmap (unwrap . unwrap)
    {-# INLINE fanInTyped #-}



------------------------------
-- === UncheckedMutable === --
------------------------------

-- | UncheckedMutable attribute allows mutable modification of the data
--   without providing any automatic sanity checking. Use it only if you are
--   sure your passes will not suffer from race condition. In case of any
--   doubts, use 'Atomic' instead.

data UncheckedMutable
type instance Wrapper UncheckedMutable = MVar

instance (MonadIO m, State.Getter (Attr attr) m)
      => TypedGetter UncheckedMutable attr m where
    getTyped = do
        mvar <- unwrap <$> State.get @(Attr attr)
        liftIO $ MVar.readMVar mvar
    {-# INLINE getTyped #-}

instance (MonadIO m, State.Getter (Attr attr) m)
      => TypedSetter UncheckedMutable attr m where
    putTyped a = do
        mvar <- unwrap <$> State.get @(Attr attr)
        liftIO $ MVar.putMVar mvar a
    {-# INLINE putTyped #-}

instance Monad m => FanInTyped UncheckedMutable attr m where
  fanInTyped (mvar :| _) = pure mvar
  {-# INLINE fanInTyped #-}
