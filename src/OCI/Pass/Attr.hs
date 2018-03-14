{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.Pass.Attr where

import           Prologue hiding (Type, Wrapped)
import qualified Prologue as P

import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar


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


-- === State management === --
-- | Attributes live in some state. We can access them or update them.
--   However, in order to change their value, we do not always have to write
--   them back to state. If they were implemented using some mutable structure
--   (like 'MVar'), we only need to read them (their reference) and mutate it.
--   Thus the attribute type determines if the 'put' is needed.

type StateEditor attr m = (StateReader attr m, StateWriter attr m)

class Monad m => StateReader attr m where
    get :: m (Attr attr)

class Monad m => StateWriter attr m where
    put :: Attr attr -> m ()


-- === Attr Reader / Writer === --

-- | Attributes can be 'read' and 'write' using these smart functions.
--   Default instances are the most common use case, when dealing with
--   immutable wrappers. However, if a mutable wrapper is used, like 'MVar'
--   there is no need to 'put' back the 'MVar' when its value is updated.

type Reader attr = TypedReader (Type attr) attr
class Monad m => TypedReader t attr m where
    read :: Type attr ~ t => m attr

type Writer attr = TypedWriter (Type attr) attr
class Monad m => TypedWriter t attr m where
    write :: Type attr ~ t => attr -> m ()


-- === Fan in / out === --

class Monad m => FanIn t attr m where
    fanIn :: t ~ Type attr => NonEmpty (Attr attr) -> m (Attr attr)

class Monad m => FanOut t attr m where
    fanOut     :: t ~ Type attr => m (Attr attr)
    fanOutMany :: t ~ Type attr => Int -> m [Attr attr]
    fanOutMany = flip replicateM fanOut ; {-# INLINE fanOutMany #-}



-----------------
-- === Rep === --
-----------------

-- === Definition === --

newtype Rep = Rep SomeTypeRep deriving (Eq, Ord, Show)
makeLenses ''Rep


-- === API === --

rep :: ∀ (attr :: T). Typeable attr => Rep
rep = wrap $ someTypeRep @attr ; {-# INLINE rep #-}

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

instance StateReader attr m => TypedReader Atomic attr m where
    read = unwrap . unwrap <$> get @attr ; {-# INLINE read #-}

instance StateWriter attr m => TypedWriter Atomic attr m where
    write = put @attr . wrap . wrap ; {-# INLINE write #-}

instance Monad m => FanIn Atomic attr m where
    fanIn = \case
        a :| [] -> pure a
        _ -> error "Impossible happened: Atomic attribute used in parallel."
    {-# INLINE fanIn #-}



-----------------------
-- === ParAppend === --
-----------------------

-- | ParAppend attribute is copied to all passes which can be run in parallel
--   and is gathered after they successfully end. The results are concatenated
--   and thus the data have to implement 'Semigroup'.

data ParAppend
type instance Wrapper ParAppend = Identity

instance StateReader attr m => TypedReader ParAppend attr m where
    read = unwrap . unwrap <$> get @attr ; {-# INLINE read #-}

instance StateWriter attr m => TypedWriter ParAppend attr m where
    write = put @attr . wrap . wrap ; {-# INLINE write #-}

instance (Monad m, Semigroup attr)
      => FanIn ParAppend attr m where
    fanIn = pure . wrap . wrap . fold1 . fmap (unwrap . unwrap)
    {-# INLINE fanIn #-}



------------------------------
-- === UncheckedMutable === --
------------------------------

-- | UncheckedMutable attribute allows mutable modification of the data
--   without providing any automatic sanity checking. Use it only if you are
--   sure your passes will not suffer from race condition. In case of any
--   doubts, use 'Atomic' instead.

data UncheckedMutable
type instance Wrapper UncheckedMutable = MVar

instance (MonadIO m, StateReader attr m)
      => TypedReader UncheckedMutable attr m where
    read = do
        mvar <- unwrap <$> get @attr
        liftIO $ MVar.readMVar mvar
    {-# INLINE read #-}

instance (MonadIO m, StateReader attr m)
      => TypedWriter UncheckedMutable attr m where
    write a = do
        mvar <- unwrap <$> get @attr
        liftIO $ MVar.putMVar mvar a
    {-# INLINE write #-}

instance Monad m => FanIn UncheckedMutable attr m where
  fanIn (mvar :| _) = pure mvar
  {-# INLINE fanIn #-}
