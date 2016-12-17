{-# LANGUAGE UndecidableInstances #-}

module System.Log.Data where

import Prologue

import           Control.Monad.State          (StateT, evalStateT)
import qualified Data.Set                     as Set
import           Data.Set                     (Set)
import qualified Control.Monad.State          as State
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import qualified GHC.Stack                    as Stack
import           Text.PrettyPrint.ANSI.Leijen (Doc, text) -- FIXME after refactor remove text import

import System.Log.Logger.Class



type IsDoc t = Convertible t Doc -- FIXME: refactor

instance Convertible String Doc where
    convert s = text s ; {-# INLINE convert #-}




-------------------------
-- === Logger Data === --
-------------------------

-- === Definitions === --

type family DataOf  a
newtype     LogData a = LogData (DataOf a)
makeWrapped ''LogData

deriving instance Eq      (DataOf a) => Eq      (LogData a)
deriving instance Ord     (DataOf a) => Ord     (LogData a)
deriving instance Enum    (DataOf a) => Enum    (LogData a)
deriving instance Num     (DataOf a) => Num     (LogData a)
deriving instance Show    (DataOf a) => Show    (LogData a)
deriving instance Default (DataOf a) => Default (LogData a)



----------------------------
-- === Data providers === --
----------------------------

-- === Definition === --

newtype DataProvider d m a = DataProvider (StateT (LogData d) m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)
makeWrapped ''DataProvider


-- === Management === --

provideData :: forall d m a. Monad m => LogData d -> DataProvider d m a -> m a
provideData = flip (evalStateT . unwrap') ; {-# INLINE provideData #-}

provideData' :: forall d m a. (Monad m, Default (LogData d)) => DataProvider d m a -> m a
provideData' = provideData def ; {-# INLINE provideData' #-}

modifyDataM :: forall d a m. DataStore d m => (LogData d -> m (a, LogData d)) -> m a
modifyDataM f = do
    d <- getData
    (a, d') <- f d
    putData d'
    return a
{-# INLINE modifyDataM #-}

modifyDataM_ :: forall d m. DataStore d m => (LogData d -> m (LogData d)) -> m ()
modifyDataM_ = modifyDataM . (fmap.fmap) ((),) ; {-# INLINE modifyDataM_ #-}

modifyData :: forall d a m. DataStore d m => (LogData d -> (a, LogData d)) -> m a
modifyData = modifyDataM . fmap return ; {-# INLINE modifyData #-}

modifyData_ :: forall d m. DataStore d m => (LogData d -> LogData d) -> m ()
modifyData_ = modifyDataM_ . fmap return ; {-# INLINE modifyData_ #-}

withData :: forall d m a. DataStore d m => LogData d -> m a -> m a
withData = withModData . const ; {-# INLINE withData #-}

withModData :: forall d m a. DataStore d m => (LogData d -> LogData d) -> m a -> m a
withModData df f = do
    old <- getData @d
    putData $ df old
    out <- f
    putData old
    return out
{-# INLINE withModData #-}


-- === DataStore === ---

class Monad m => DataStore d m where
    getData :: m (LogData d)
    putData :: LogData d -> m ()

instance Monad m => DataStore d (DataProvider d m) where
    getData = wrap'   State.get ; {-# INLINE getData #-}
    putData = wrap' . State.put ; {-# INLINE putData #-}

type DataStoreTrans d t m = (Monad m, MonadTrans t, Monad (t m), DataStore d m)
instance {-# OVERLAPPABLE #-} DataStoreTrans d t m => DataStore d (t m) where
    getData = lift   getData ; {-# INLINE getData #-}
    putData = lift . putData ; {-# INLINE putData #-}

type family DataStores ds   m :: Constraint where
    DataStores '[] m = ()
    DataStores (d ': ds) m = (DataStore d m, DataStores ds   m)


getData' :: forall d m. DataStore d m => m (DataOf d)
getData' = unwrap' <$> getData @d ; {-# INLINE getData' #-}

putData' :: forall d m. DataStore d m => DataOf d -> m ()
putData' = putData @d . wrap' ; {-# INLINE putData' #-}


-- === Instances === --

instance MonadLogging m => MonadLogging (DataProvider d m)

instance PrimMonad m => PrimMonad (DataProvider d m) where
    type PrimState (DataProvider d m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}



------------------------------
-- === BaseLogger datas === --
------------------------------


-- === Loc === --

data Loc = Loc deriving (Show)
type instance DataOf Loc = Stack.SrcLoc

loc :: Stack.SrcLoc -> LogData Loc
loc = wrap' ; {-# INLINE loc #-}


-- === Time === --

data Time = Time deriving (Show)
type instance DataOf Time = UTCTime

time :: UTCTime -> LogData Time
time = wrap' ; {-# INLINE time #-}

instance DataStore Time IO where
    getData = time <$> getCurrentTime ; {-# INLINE getData #-}
    putData = error "impossible"      ; {-# INLINE putData #-}


-- === Msg === --

data Msg = Msg deriving (Show)
type instance DataOf Msg = Doc

msg :: IsDoc t => t -> LogData Msg
msg = wrap' . convert ; {-# INLINE msg #-}


-- === Reporter === --

data Reporter = Reporter deriving (Show)
type instance DataOf Reporter = Text

reporter :: ToText r => r -> LogData Reporter
reporter = wrap' . convert ; {-# INLINE reporter #-}

reported :: (DataStore Reporter m, ToText r) => r -> m a -> m a
reported = withData . reporter ; {-# INLINE reported #-}


-- === DynTags === --

type DynTag = TypeRep

data DynTags = DynTags deriving (Show)
type instance DataOf DynTags = Set DynTag

dynTag :: Typeable p => p -> DynTag
dynTag = typeOf ; {-# INLINE dynTag #-}

dynTags :: Set DynTag -> LogData DynTags
dynTags = wrap' ; {-# INLINE dynTags #-}


instance (Monad m, Typeable tag) => MonadTag tag (DataProvider DynTags m) where
    setTag   = modifyData_ @DynTags $ wrapped' %~ Set.insert (typeRep' @tag) ; {-# INLINE setTag   #-}
    unsetTag = modifyData_ @DynTags $ wrapped' %~ Set.delete (typeRep' @tag) ; {-# INLINE unsetTag #-}


-- === Priority === --

data Priority = Priority deriving (Show)
type instance DataOf Priority = Int

priority :: Enum p => p -> LogData Priority
priority = wrap' . fromEnum ; {-# INLINE priority #-}


-- === Nesting === --

data Nesting = Nesting deriving (Show)
type instance DataOf Nesting = Int

nesting :: Int -> LogData Nesting
nesting = wrap' ; {-# INLINE nesting #-}

withNesting :: DataStore Nesting m => (LogData Nesting -> LogData Nesting) -> m ()
withNesting = modifyData_ @Nesting ; {-# INLINE withNesting #-}

incNesting :: DataStore Nesting m => m ()
incNesting = withNesting succ ; {-# INLINE incNesting #-}

decNesting :: DataStore Nesting m => m ()
decNesting = withNesting pred ; {-# INLINE decNesting #-}

nested :: DataStore Nesting m => m a -> m a
nested = withModData @Nesting succ ; {-# INLINE nested #-}
