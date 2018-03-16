{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.Pass.Attr where

import           Prologue hiding (Type, Wrapped, read)
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


-- === Instances === --

deriving instance Show (Wrapped attr attr) => Show (Attr attr)


-- === State management === --
-- | Attributes live in some state. We can access them or update them.
--   However, in order to change their value, we do not always have to write
--   them back to state. If they were implemented using some mutable structure
--   (like 'MVar'), we only need to read them (their reference) and mutate it.
--   Thus the attribute type determines if the 'write' is needed.

type Editor attr m = (Reader attr m, Writer attr m)

class Monad m => Reader attr m where
    read :: m (Attr attr)

class Monad m => Writer attr m where
    write :: Attr attr -> m ()


-- === Attr Reader / Writer === --

-- | Attributes can be 'read' and 'write' using these smart functions.
--   Default instances are the most common use case, when dealing with
--   immutable wrappers. However, if a mutable wrapper is used, like 'MVar'
--   there is no need to 'write' back the 'MVar' when its value is updated.

class Monad m => Getter     attr m where get   :: m attr
class Monad m => Setter     attr m where put   :: attr -> m ()
class Monad m => Getter__ t attr m where get__ :: Type attr ~ t => m attr
class Monad m => Setter__ t attr m where put__ :: Type attr ~ t => attr -> m ()

instance (Monad m, Getter__ (Type attr) attr m)
      => Getter attr m where get = get__ ; {-# INLINE get #-}

instance (Monad m, Setter__ (Type attr) attr m)
      => Setter attr m where put = put__ ; {-# INLINE put #-}

instance {-# OVERLAPPABLE #-} Monad m => Getter Imp  m where get = impossible
instance {-# OVERLAPPABLE #-} Monad m => Setter Imp  m where put = impossible
instance {-# OVERLAPPABLE #-} Getter attr ImpM where get = impossible
instance {-# OVERLAPPABLE #-} Setter attr ImpM where put = impossible


-- === Fan in / out === --

type FanIn attr = FanIn__ (Type attr) attr
class Monad m => FanIn__ t attr m where
    fanIn__ :: t ~ Type attr => NonEmpty (Attr attr) -> m (Attr attr)

class Monad m => FanOut__ t attr m where
    fanOut     :: t ~ Type attr => m (Attr attr)
    fanOutMany :: t ~ Type attr => Int -> m [Attr attr]
    fanOutMany = flip replicateM fanOut ; {-# INLINE fanOutMany #-}


fanIn :: ∀ attr m. FanIn attr m => NonEmpty (Attr attr) -> m (Attr attr)
fanIn = fanIn__ ; {-# INLINE fanIn #-}



-----------------
-- === Rep === --
-----------------

-- === Definition === --

newtype Rep = Rep SomeTypeRep deriving (Eq, Ord, Show)
makeLenses ''Rep


-- === API === --

rep :: ∀ (attr :: T). Typeable attr => Rep
rep = wrap $ someTypeRep @attr ; {-# INLINE rep #-}

reps :: ∀ (attrs :: [T]). Typeables attrs => [Rep]
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

instance Reader attr m => Getter__ Atomic attr m where
    get__ = unwrap . unwrap <$> read @attr ; {-# INLINE get__ #-}

instance Writer attr m => Setter__ Atomic attr m where
    put__ = write @attr . wrap . wrap ; {-# INLINE put__ #-}

instance Monad m => FanIn__ Atomic attr m where
    fanIn__ = \case
        a :| [] -> pure a
        _ -> error "Impossible happened: Atomic attribute used in parallel."
    {-# INLINE fanIn__ #-}



-----------------------
-- === ParAppend === --
-----------------------

-- | ParAppend attribute is copied to all passes which can be run in parallel
--   and is gathered after they successfully end. The results are concatenated
--   and thus the data have to implement 'Semigroup'.

data ParAppend
type instance Wrapper ParAppend = Identity

instance Reader attr m => Getter__ ParAppend attr m where
    get__ = unwrap . unwrap <$> read @attr ; {-# INLINE get__ #-}

instance Writer attr m => Setter__ ParAppend attr m where
    put__ = write @attr . wrap . wrap ; {-# INLINE put__ #-}

instance (Monad m, Semigroup attr)
      => FanIn__ ParAppend attr m where
    fanIn__ = pure . wrap . wrap . fold1 . fmap (unwrap . unwrap)
    {-# INLINE fanIn__ #-}



------------------------------
-- === UncheckedMutable === --
------------------------------

-- | UncheckedMutable attribute allows mutable modification of the data
--   without providing any automatic sanity checking. Use it only if you are
--   sure your passes will not suffer from race condition. In case of any
--   doubts, use 'Atomic' instead.

data UncheckedMutable
type instance Wrapper UncheckedMutable = MVar

instance (MonadIO m, Reader attr m)
      => Getter__ UncheckedMutable attr m where
    get__ = do
        mvar <- unwrap <$> read @attr
        liftIO $ MVar.readMVar mvar
    {-# INLINE get__ #-}

instance (MonadIO m, Reader attr m)
      => Setter__ UncheckedMutable attr m where
    put__ a = do
        mvar <- unwrap <$> read @attr
        liftIO $ MVar.putMVar mvar a
    {-# INLINE put__ #-}

instance Monad m => FanIn__ UncheckedMutable attr m where
  fanIn__ (mvar :| _) = pure mvar
  {-# INLINE fanIn__ #-}
