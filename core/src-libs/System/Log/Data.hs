{-# LANGUAGE UndecidableInstances #-}

module System.Log.Data where

import Prologue

import           Control.Monad.State                  (StateT, evalStateT)
import           Control.Monad.Error.Class            (MonadError)
import           Control.Monad.Cont.Class             (MonadCont)
import           Control.Monad.Catch                  (MonadThrow, MonadCatch)
import qualified Data.Set                             as Set
import           Data.Set                             (Set)
import qualified Control.Monad.State                  as State
import           Data.Time.Clock                      (UTCTime, getCurrentTime)
import qualified GHC.Stack                            as Stack
import           Text.PrettyPrint.ANSI.Leijen         (Doc)
import           Text.PrettyPrint.ANSI.Leijen.Convert
import System.Log.Logger.Class



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

newtype DataProvider d m a = DataProvider (StateT (LogData d) m a)
                             deriving ( Functor, Applicative, Alternative, Monad, MonadTrans, MonadIO, MonadFix
                                      , MonadError e, MonadPlus, MonadCont, MonadThrow, MonadCatch)
makeWrapped ''DataProvider

type family DataProviderStack ps m where
    DataProviderStack (p ': ps) m = DataProvider p (DataProviderStack ps m)
    DataProviderStack '[]       m = m


-- === Management === --

provideData :: forall d m a. Monad m => LogData d -> DataProvider d m a -> m a
provideData = flip (evalStateT . unwrap') 

provideDefData :: forall d m a. (Monad m, Default (LogData d)) => DataProvider d m a -> m a
provideDefData = provideData def 

modifyDataM :: forall d a m. DataStore d m => (LogData d -> m (a, LogData d)) -> m a
modifyDataM f = do
    d <- getData
    (a, d') <- f d
    putData d'
    return a
{-# INLINE modifyDataM #-}

modifyDataM_ :: forall d m. DataStore d m => (LogData d -> m (LogData d)) -> m ()
modifyDataM_ = modifyDataM . (fmap.fmap) ((),) 

modifyData :: forall d a m. DataStore d m => (LogData d -> (a, LogData d)) -> m a
modifyData = modifyDataM . fmap return 

modifyData_ :: forall d m. DataStore d m => (LogData d -> LogData d) -> m ()
modifyData_ = modifyDataM_ . fmap return 

withData :: forall d m a. DataStore d m => LogData d -> m a -> m a
withData = withModData . const 

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
    getData = wrap'   State.get 
    putData = wrap' . State.put 

type DataStoreTrans d t m = (Monad m, MonadTrans t, Monad (t m), DataStore d m)
instance {-# OVERLAPPABLE #-} DataStoreTrans d t m => DataStore d (t m) where
    getData = lift   getData 
    putData = lift . putData 

type family DataStores ds   m :: Constraint where
    DataStores '[] m = ()
    DataStores (d ': ds) m = (DataStore d m, DataStores ds   m)


getData' :: forall d m. DataStore d m => m (DataOf d)
getData' = unwrap' <$> getData @d 

putData' :: forall d m. DataStore d m => DataOf d -> m ()
putData' = putData @d . wrap' 


-- === Instances === --

instance MonadLogging m => MonadLogging (DataProvider d m)

instance PrimMonad m => PrimMonad (DataProvider d m) where
    type PrimState (DataProvider d m) = PrimState m
    primitive = lift . primitive 



------------------------------
-- === BaseLogger datas === --
------------------------------


-- === Loc === --

data Loc = Loc deriving (Show)
type instance DataOf Loc = Stack.SrcLoc

loc :: Stack.SrcLoc -> LogData Loc
loc = wrap' 


-- === Time === --

data Time = Time deriving (Show)
type instance DataOf Time = UTCTime

time :: UTCTime -> LogData Time
time = wrap' 

instance DataStore Time IO where
    getData = time <$> getCurrentTime 
    putData = error "impossible"      


-- === Msg === --

data Msg = Msg deriving (Show)
type instance DataOf Msg = Doc

msg :: Doc -> LogData Msg
msg = wrap' 


-- === Reporter === --

data Reporter = Reporter deriving (Show)
type instance DataOf Reporter = Text

reporter :: Text -> LogData Reporter
reporter = wrap' 

reported :: DataStore Reporter m => Text -> m a -> m a
reported = withData . reporter 


-- === DynTags === --

type DynTag = TypeRep

data DynTags = DynTags deriving (Show)
type instance DataOf DynTags = Set DynTag

dynTag :: Typeable p => p -> DynTag
dynTag = typeOf 

dynTags :: Set DynTag -> LogData DynTags
dynTags = wrap' 


instance (MonadTagged tag m, Typeable tag) => MonadTagged tag (DataProvider DynTags m) where
    preTagged  = modifyData_ @DynTags (wrapped' %~ Set.insert (typeRep' @tag)) >> lift (preTagged  @tag) 
    postTagged = modifyData_ @DynTags (wrapped' %~ Set.delete (typeRep' @tag)) >> lift (postTagged @tag) 
    inTagged   = lift . inTagged @tag                                                                    


-- === Priority === --

data Priority = Priority deriving (Show)
type instance DataOf Priority = Int

priority :: Enum p => p -> LogData Priority
priority = wrap' . fromEnum 


-- === Nesting === --

data Nesting = Nesting deriving (Show)
type instance DataOf Nesting = Int

nesting :: Int -> LogData Nesting
nesting = wrap' 

withNesting :: DataStore Nesting m => (LogData Nesting -> LogData Nesting) -> m ()
withNesting = modifyData_ @Nesting 

incNesting :: DataStore Nesting m => m ()
incNesting = withNesting succ 

decNesting :: DataStore Nesting m => m ()
decNesting = withNesting pred 

nested :: DataStore Nesting m => m a -> m a
nested = withModData @Nesting succ 
