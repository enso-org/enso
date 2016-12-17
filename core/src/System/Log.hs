{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE NoOverloadedStrings  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs                #-}

module System.Log where

import Data.Foldable (fold)

import           Prologue hiding (log, nested, pprint)
import           Data.Text                    (Text)
import qualified Control.Monad.State          as State
import           Control.Monad.State          (StateT, runStateT)
import           Control.Monad.Identity       (Identity)
import qualified Control.Monad.Trans.Identity as Identity
import           Control.Monad.Trans.Identity (IdentityT, runIdentityT)
import           Data.List                    (elemIndex)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import qualified GHC.Prim                     as Prim

import Data.Time.Clock              (getCurrentTime, UTCTime)
import Data.Time.Format             (formatTime)
import Data.Time.Locale.Compat      (defaultTimeLocale)
import Text.PrettyPrint.ANSI.Leijen (Doc, Pretty, text, pretty, putDoc)
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

import qualified Data.Set as Set
import           Data.Set (Set)

import qualified Data.Typeable as Typeable
import GHC.Stack
import qualified Type.List as List


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



--------------------
-- === Logger === --
--------------------

data family Logger l (m :: * -> *) a

instance (MonadTrans (Logger l), Monad (Logger l m), PrimMonad m)
      => PrimMonad (Logger l m) where
    type PrimState (Logger l m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}

class IsLogger l m where
    runLogger :: Logger l m ()
    default runLogger :: Monad (Logger l m) => Logger l m ()
    runLogger = return ()


-- === LoggersFinder === --

class Monad m => LoggersFinder m where
    findLoggers :: m ()

instance {-# OVERLAPPABLE #-}
         LoggersFinderTrans t m => LoggersFinder (t m)        where findLoggers = lift findLoggers              ; {-# INLINE findLoggers #-}
instance LoggersFinderFound l m => LoggersFinder (Logger l m) where findLoggers = runLogger >> lift findLoggers ; {-# INLINE findLoggers #-}
instance                           LoggersFinder IO           where findLoggers = return ()                     ; {-# INLINE findLoggers #-}
instance                           LoggersFinder Identity     where findLoggers = return ()                     ; {-# INLINE findLoggers #-}
type LoggersFinderFound l m = (LoggersFinderTrans (Logger l) m, IsLogger l m, LoggersFinder m)
type LoggersFinderTrans t m = (Monad m, Monad (t m), MonadTrans t, LoggersFinder m)



---------------------------
-- === Data handling === --
---------------------------

-- === Definition === --

newtype DataProvider d m a = DataProvider (StateT (LogData d) m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)
makeWrapped ''DataProvider


-- === Management === --

provideData :: forall d m a. Monad m => LogData d -> DataProvider d m a -> m a
provideData = flip (State.evalStateT . unwrap') ; {-# INLINE provideData #-}

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
    DataStores '[  ] m = ()
    DataStores (d   ': ds) m = (DataStore d m, DataStores ds   m)


getData' :: forall d m. DataStore d m => m (DataOf d)
getData' = unwrap' <$> getData @d ; {-# INLINE getData' #-}

putData' :: forall d m. DataStore d m => DataOf d -> m ()
putData' = putData @d . wrap' ; {-# INLINE putData' #-}


-- === Instances === --

instance PrimMonad m => PrimMonad (DataProvider d m) where
    type PrimState (DataProvider d m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}




--------------------------
-- === MonadLogging === --
--------------------------

-- | The whle purpose of MonadLogging is to give nice compilation errors.
--   After Logger is seen, we fire data discovery and logger execution.

-- === Definition === --

class Monad m => MonadLogging m where
    log :: m ()
    default log :: (MonadTrans t, MonadLogging m) => t m ()
    log = lift log ; {-# INLINE log #-}


-- === Instances === --

instance (Monad m, LoggersFinder (DataProvider d m))
      => MonadLogging (DataProvider d m) where
    log = findLoggers ; {-# INLINE log #-}

instance (Monad m, Monad (Logger l m), LoggersFinder (Logger l m))
      => MonadLogging (Logger l m) where
    log = findLoggers ; {-# INLINE log #-}

-- Standard monads
instance MonadLogging m => MonadLogging (StateT s m)
instance MonadLogging m => MonadLogging (IdentityT m)




--------------------------------------------------------
--------------------------------------------------------



type IsDoc t = Convertible t Doc -- FIXME: refactor

instance Convertible String Doc where
    convert s = Doc.text s ; {-# INLINE convert #-}


-------------------------------------
-- === Standard logging levels === --
-------------------------------------

data Debug    = Debug    deriving (Show) -- Debug Logs
data Info     = Info     deriving (Show) -- Information
data Notice   = Notice   deriving (Show) -- Normal runtime conditions
data Warning  = Warning  deriving (Show) -- General Warnings
data Error    = Error    deriving (Show) -- General Errors
data Critical = Critical deriving (Show) -- Severe situations
data Alert    = Alert    deriving (Show) -- Take immediate action
data Panic    = Panic    deriving (Show) -- System is unusable

type StdLogLevels = '[Debug, Info, Notice, Warning, Error, Critical, Alert, Panic]


------------------------------
-- === BaseLogger datas === --
------------------------------


-- === Loc === --

data Loc = Loc deriving (Show)
type instance DataOf Loc = SrcLoc

loc :: SrcLoc -> LogData Loc
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



------------------------------------
-- === Standard logging utils === --
------------------------------------

-- === Types === --

type MultiLogging ls m = (MonadLogging m, DataStores ls      m)
type LocLogging      m = (MonadLogging m, DataStore Loc      m)
type MsgLogging      m = (MonadLogging m, DataStore Msg      m)
type PriorityLogging m = (MsgLogging   m, DataStore Priority m)
type ReportedLogging m = (MsgLogging   m, DataStore Reporter m)
type NestedLogging   m = (MsgLogging   m, DataStore Nesting  m)


type Logging        m = (LocLogging m, ReportedLogging m, NestedLogging m, MonadTags StdLogLevels m)


-- === Generic logging === --

addCallStackInfo :: (HasCallStack, DataStore Loc m) => m a -> m a
addCallStackInfo = withData (loc . snd . head . drop 1 $ getCallStack callStack)

-- priLog' :: (HasCallStack, MultiLogging '[Priority, Msg] m, Enum priority, IsDoc msg)
--        => priority -> msg -> m ()
-- priLog' p m = withData (priority p) $ withData (msg m) log




type LocLog m = (HasCallStack, DataStore Loc m)

type     MsgLog'              msg m = (MonadLogging      m, DataStore Msg      m, IsDoc msg)
type     TagLog'   t          msg m = (MsgLog'       msg m, MonadTag  t        m)
type     TagLogBy' t reporter msg m = (TagLog'     t msg m, DataStore Reporter m, ToText reporter)
type WithTagLog'   t          msg m = (TagLog'     t msg m, DataStore Nesting  m)
type WithTagLogBy' t reporter msg m = (WithTagLog' t msg m, DataStore Reporter m, ToText reporter)

type     TagLog    t          msg m = (LocLog m,     TagLog'   t          msg m)
type     TagLogBy  t reporter msg m = (LocLog m,     TagLogBy' t reporter msg m)
type WithTagLog    t          msg m = (LocLog m, WithTagLog'   t          msg m)
type WithTagLogBy  t reporter msg m = (LocLog m, WithTagLogBy' t reporter msg m)


msgLog' :: forall msg m. MsgLog' msg m
        => msg -> m ()
msgLog' m = withData (msg m) log ; {-# INLINE msgLog' #-}

tagLog' :: forall t msg m. TagLog' t msg m
        => msg -> m ()
tagLog' = tagged @t . msgLog' ; {-# INLINE tagLog' #-}

tagLogBy' :: forall t reporter msg m. TagLogBy' t reporter msg m
          => reporter -> msg -> m ()
tagLogBy' r = reported r . tagLog' @t ; {-# INLINE tagLogBy' #-}

withTagLog' :: forall t msg m a. WithTagLog' t msg m
            => msg -> m a -> m a
withTagLog' m f = tagLog' @t m >> nested f ; {-# INLINE withTagLog' #-}

withTagLogBy' :: forall t reporter msg m a. WithTagLogBy' t reporter msg m
              => reporter -> msg -> m a -> m a
withTagLogBy' r m f = tagLogBy' @t r m >> nested f ; {-# INLINE withTagLogBy' #-}


-- === Loggign utils === --

debug    :: TagLog Debug    msg m => msg -> m ()
info     :: TagLog Info     msg m => msg -> m ()
notice   :: TagLog Notice   msg m => msg -> m ()
warning  :: TagLog Warning  msg m => msg -> m ()
err      :: TagLog Error    msg m => msg -> m ()
critical :: TagLog Critical msg m => msg -> m ()
alert    :: TagLog Alert    msg m => msg -> m ()
panic    :: TagLog Panic    msg m => msg -> m ()

debug    = addCallStackInfo . tagLog' @Debug    ; {-# INLINE debug    #-}
info     = addCallStackInfo . tagLog' @Info     ; {-# INLINE info     #-}
notice   = addCallStackInfo . tagLog' @Notice   ; {-# INLINE notice   #-}
warning  = addCallStackInfo . tagLog' @Warning  ; {-# INLINE warning  #-}
err      = addCallStackInfo . tagLog' @Error    ; {-# INLINE err      #-}
critical = addCallStackInfo . tagLog' @Critical ; {-# INLINE critical #-}
alert    = addCallStackInfo . tagLog' @Alert    ; {-# INLINE alert    #-}
panic    = addCallStackInfo . tagLog' @Panic    ; {-# INLINE panic    #-}


debugBy    :: TagLogBy Debug    reporter msg m => reporter -> msg -> m ()
infoBy     :: TagLogBy Info     reporter msg m => reporter -> msg -> m ()
noticeBy   :: TagLogBy Notice   reporter msg m => reporter -> msg -> m ()
warningBy  :: TagLogBy Warning  reporter msg m => reporter -> msg -> m ()
errBy      :: TagLogBy Error    reporter msg m => reporter -> msg -> m ()
criticalBy :: TagLogBy Critical reporter msg m => reporter -> msg -> m ()
alertBy    :: TagLogBy Alert    reporter msg m => reporter -> msg -> m ()
panicBy    :: TagLogBy Panic    reporter msg m => reporter -> msg -> m ()

debugBy    = addCallStackInfo .: tagLogBy' @Debug    ; {-# INLINE debugBy    #-}
infoBy     = addCallStackInfo .: tagLogBy' @Info     ; {-# INLINE infoBy     #-}
noticeBy   = addCallStackInfo .: tagLogBy' @Notice   ; {-# INLINE noticeBy   #-}
warningBy  = addCallStackInfo .: tagLogBy' @Warning  ; {-# INLINE warningBy  #-}
errBy      = addCallStackInfo .: tagLogBy' @Error    ; {-# INLINE errBy      #-}
criticalBy = addCallStackInfo .: tagLogBy' @Critical ; {-# INLINE criticalBy #-}
alertBy    = addCallStackInfo .: tagLogBy' @Alert    ; {-# INLINE alertBy    #-}
panicBy    = addCallStackInfo .: tagLogBy' @Panic    ; {-# INLINE panicBy    #-}


withDebug    :: WithTagLog Debug    msg m => msg -> m a -> m a
withInfo     :: WithTagLog Info     msg m => msg -> m a -> m a
withNotice   :: WithTagLog Notice   msg m => msg -> m a -> m a
withWarning  :: WithTagLog Warning  msg m => msg -> m a -> m a
withErr      :: WithTagLog Error    msg m => msg -> m a -> m a
withCritical :: WithTagLog Critical msg m => msg -> m a -> m a
withAlert    :: WithTagLog Alert    msg m => msg -> m a -> m a
withPanic    :: WithTagLog Panic    msg m => msg -> m a -> m a

withDebug    = addCallStackInfo .: withTagLog' @Debug    ; {-# INLINE withDebug    #-}
withInfo     = addCallStackInfo .: withTagLog' @Info     ; {-# INLINE withInfo     #-}
withNotice   = addCallStackInfo .: withTagLog' @Notice   ; {-# INLINE withNotice   #-}
withWarning  = addCallStackInfo .: withTagLog' @Warning  ; {-# INLINE withWarning  #-}
withErr      = addCallStackInfo .: withTagLog' @Error    ; {-# INLINE withErr      #-}
withCritical = addCallStackInfo .: withTagLog' @Critical ; {-# INLINE withCritical #-}
withAlert    = addCallStackInfo .: withTagLog' @Alert    ; {-# INLINE withAlert    #-}
withPanic    = addCallStackInfo .: withTagLog' @Panic    ; {-# INLINE withPanic    #-}


withDebugBy    :: WithTagLogBy Debug    reporter msg m => reporter -> msg -> m a -> m a
withInfoBy     :: WithTagLogBy Info     reporter msg m => reporter -> msg -> m a -> m a
withNoticeBy   :: WithTagLogBy Notice   reporter msg m => reporter -> msg -> m a -> m a
withWarningBy  :: WithTagLogBy Warning  reporter msg m => reporter -> msg -> m a -> m a
withErrBy      :: WithTagLogBy Error    reporter msg m => reporter -> msg -> m a -> m a
withCriticalBy :: WithTagLogBy Critical reporter msg m => reporter -> msg -> m a -> m a
withAlertBy    :: WithTagLogBy Alert    reporter msg m => reporter -> msg -> m a -> m a
withPanicBy    :: WithTagLogBy Panic    reporter msg m => reporter -> msg -> m a -> m a

withDebugBy    = addCallStackInfo .:. withTagLogBy' @Debug    ; {-# INLINE withDebugBy    #-}
withInfoBy     = addCallStackInfo .:. withTagLogBy' @Info     ; {-# INLINE withInfoBy     #-}
withNoticeBy   = addCallStackInfo .:. withTagLogBy' @Notice   ; {-# INLINE withNoticeBy   #-}
withWarningBy  = addCallStackInfo .:. withTagLogBy' @Warning  ; {-# INLINE withWarningBy  #-}
withErrBy      = addCallStackInfo .:. withTagLogBy' @Error    ; {-# INLINE withErrBy      #-}
withCriticalBy = addCallStackInfo .:. withTagLogBy' @Critical ; {-# INLINE withCriticalBy #-}
withAlertBy    = addCallStackInfo .:. withTagLogBy' @Alert    ; {-# INLINE withAlertBy    #-}
withPanicBy    = addCallStackInfo .:. withTagLogBy' @Panic    ; {-# INLINE withPanicBy    #-}


-- #ifdef IgnoreDebug

-- === Running utils === --

runPriorityLogging :: forall req m a. Monad m
           => DataProvider Loc
            ( DataProvider Msg
            $ DataProvider Reporter
            $ DataProvider Nesting
            $ DataProvider Priority m
            ) a -> m a
runPriorityLogging = provideData 0 -- FIXME: Is there any better way to provide non-yet-set priority?
           . provideData' @Nesting
           . provideData (reporter "")
           . provideData (msg "")
           . provideData (loc $ error "No source location provided")
{-# INLINE runPriorityLogging #-}

runTaggedLogging = provideData  (dynTags mempty)
                 . provideData' @Nesting
                 . provideData  (reporter "")
                 . provideData  (msg "")
                 . provideData  (loc $ error "No source location provided")


-----------------------
-- === MonadTags === --
-----------------------

-- === Definition === --

class Monad m => MonadTag tag m where
    setTag   :: m ()
    unsetTag :: m ()

type family MonadTags ts m :: Constraint where
    MonadTags '[]       m = ()
    MonadTags (t ': ts) m = (MonadTag t m, MonadTags ts m)


-- === Utils === --

tagged :: forall t m a. MonadTag t m => m a -> m a
tagged f = setTag @t *> f <* unsetTag @t ; {-# INLINE tagged #-}


-- === Instances === --

instance {-# OVERLAPPABLE #-} (MonadTag tag m, Monad m, Monad (t m), MonadTrans t)
      => MonadTag tag (t m) where
    setTag   = lift $ setTag   @tag ; {-# INLINE setTag   #-}
    unsetTag = lift $ unsetTag @tag ; {-# INLINE unsetTag #-}

instance MonadTag tag IO where
    setTag   = return () ; {-# INLINE setTag   #-}
    unsetTag = return () ; {-# INLINE unsetTag #-}

instance MonadTag tag Identity where
    setTag   = return () ; {-# INLINE setTag   #-}
    unsetTag = return () ; {-# INLINE unsetTag #-}




------------------------
-- === EchoLogger === --
------------------------

data Echo
type EchoLogger = Logger Echo

newtype instance Logger Echo m a = EchoLogger { fromEchoLogger :: IdentityT m a}
        deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadTrans)

runEchoLogger :: EchoLogger m a -> m a
runEchoLogger = runIdentityT . fromEchoLogger ; {-# INLINE runEchoLogger #-}


-- === Instances === --

instance (MonadIO m, DataStore Msg m) => IsLogger Echo m where
    runLogger = liftIO . putDoc =<< getData' @Msg ; {-# INLINE runLogger #-}



----------------------------
-- === PriorityLogger === --
----------------------------

data PRIORITY (prs :: [*])
type PriorityLogger prs = Logger (PRIORITY prs)
instance Monad m => IsLogger (PRIORITY prs) m

newtype instance Logger (PRIORITY prs) m a = PriorityLogger { fromPriorityLogger :: IdentityT m a}
        deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadTrans)

runPriorityLogger :: forall prs m a. (PriorityLogger prs) m a -> m a
runPriorityLogger = runIdentityT . fromPriorityLogger ; {-# INLINE runPriorityLogger #-}


-- === Tag priority handling === --

instance (maybeIdx ~ List.Index tag prs, HandlePriorityTag maybeIdx m)
      => MonadTag tag (PriorityLogger prs m) where
    setTag   = handlePriorityTag @maybeIdx ; {-# INLINE setTag   #-}
    unsetTag = return ()                   ; {-# INLINE unsetTag #-}

class Monad m => HandlePriorityTag (idx :: Maybe Nat) m where
    handlePriorityTag :: forall prs. PriorityLogger prs m ()

instance Monad m => HandlePriorityTag 'Nothing m where
    handlePriorityTag = return () ; {-# INLINE handlePriorityTag #-}

instance (DataStore Priority m, KnownNat n) => HandlePriorityTag ('Just n) m where
    handlePriorityTag = putData $ priority $ natVal (Proxy :: Proxy n) ; {-# INLINE handlePriorityTag #-}



--------------------------
-- === WriterLogger === --
--------------------------

-- === Definition === --

data Writer t
type WriterLogger t = Logger (Writer t)

newtype instance Logger (Writer t) m a = WriterLogger { fromWriterLogger :: StateT [LogData t] m a}
        deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadTrans)

execWriterLogger :: forall t m a. Monad m => WriterLogger t m a -> m [DataOf t]
execWriterLogger = (fmap.fmap) unwrap' . flip State.execStateT mempty . fromWriterLogger ; {-# INLINE execWriterLogger #-}


-- === MonadWriterLoger === --

class Monad m => MonadWriterLoger t m where
    getLogs :: m [LogData t]
    putLogs :: [LogData t] -> m ()

instance Monad m => MonadWriterLoger d (WriterLogger d m) where
    getLogs = WriterLogger   State.get ; {-# INLINE getLogs #-}
    putLogs = WriterLogger . State.put ; {-# INLINE putLogs #-}

instance {-# OVERLAPPABLE #-} (Monad m, Monad (t m), MonadTrans t, MonadWriterLoger d m)
      => MonadWriterLoger d (t m) where
    getLogs = lift   getLogs ; {-# INLINE getLogs #-}
    putLogs = lift . putLogs ; {-# INLINE putLogs #-}


-- === Modifications === --

modifyLogsM :: forall d a m. MonadWriterLoger d m => ([LogData d] -> m (a, [LogData d])) -> m a
modifyLogsM f = do
    d <- getLogs
    (a, d') <- f d
    putLogs d'
    return a
{-# INLINE modifyLogsM #-}

modifyLogsM_ :: forall d m. MonadWriterLoger d m => ([LogData d] -> m ([LogData d])) -> m ()
modifyLogsM_ = modifyLogsM . (fmap.fmap) ((),) ; {-# INLINE modifyLogsM_ #-}

modifyLogs :: forall d a m. MonadWriterLoger d m => ([LogData d] -> (a, [LogData d])) -> m a
modifyLogs = modifyLogsM . fmap return ; {-# INLINE modifyLogs #-}

modifyLogs_ :: forall d m. MonadWriterLoger d m => ([LogData d] -> [LogData d]) -> m ()
modifyLogs_ = modifyLogsM_ . fmap return ; {-# INLINE modifyLogs_ #-}


-- === Instances === --

instance DataStore t m => IsLogger (Writer t) m where
    runLogger = modifyLogs_ . (:) =<< getData @t ; {-# INLINE runLogger #-}



------------------------
-- === DropLogger === --
------------------------

data Drop
type DropLogger = Logger Drop

newtype instance Logger Drop m a = DropLogger { fromDropLogger :: IdentityT m a}
        deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadTrans)

dropLogs :: DropLogger m a -> m a
dropLogs = runIdentityT . fromDropLogger ; {-# INLINE dropLogs #-}


-- === Instances === --

instance {-# OVERLAPPING #-}
         Monad m => MonadLogging  (DropLogger m) where log         = return () ; {-# INLINE log         #-}
instance Monad m => LoggersFinder (DropLogger m) where findLoggers = return () ; {-# INLINE findLoggers #-}

-- This is hacky, but because Drop logger is guaranteed to drop all logs,
-- we can be sure the information will not be needed.
instance Monad m => DataStore d (DropLogger m) where
    getData   = return (error "impossible") ; {-# INLINE getData #-}
    putData _ = return (error "impossible") ; {-# INLINE putData #-}






--------------------------
-- === FormatLogger === --
--------------------------

-- | Formats the log and puts the result into Msg field.

data Format
type FormatLogger = Logger Format

newtype instance Logger Format m a = FormatLogger { fromFormatLogger :: StateT (Formatter (FormatLogger m)) m a}
        deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

instance DataStore Msg m => IsLogger Format m where
    runLogger = putData' @Msg =<< runFormatter =<< getFormatter ; {-# INLINE runLogger #-}

runFormatLogger :: Monad m => Formatter m ->FormatLogger m a -> m a
runFormatLogger f l = State.evalStateT (fromFormatLogger l) $ liftFormatter f ; {-# INLINE runFormatLogger #-}


-- === MonadLogFormatter === --

class Monad m => MonadLogFormatter m where
    getFormatter :: m (Formatter m)
    putFormatter :: Formatter m -> m ()

instance Monad m => MonadLogFormatter (FormatLogger m) where
    getFormatter = FormatLogger   State.get ; {-# INLINE getFormatter #-}
    putFormatter = FormatLogger . State.put ; {-# INLINE putFormatter #-}


-- === Instances === --

instance MonadTrans (Logger Format) where
    lift = FormatLogger . lift ; {-# INLINE lift #-}

---

-- === Formatter === --

newtype Formatter  m = Formatter  { runFormatter  ::        m Doc }
newtype FormatterT m = FormatterT { runFormatterT :: Doc -> m Doc }

liftFormatter :: (Monad m, MonadTrans t) => Formatter m -> Formatter (t m)
liftFormatter = Formatter . lift . runFormatter ; {-# INLINE liftFormatter #-}


-- === FormatterBuilder ===

infixr 5 <:>
(<:>) :: Monad m => (FormatterTBuilder a m, FormatterBuilder b m) => a -> b -> Formatter m
(<:>) a b = applyFormatters (buildFormatterT a) (buildFormatter b)

applyFormatters :: Monad m => FormatterT m -> Formatter m -> Formatter m
applyFormatters (FormatterT f) (Formatter g) = Formatter (f =<< g) ; {-# INLINE applyFormatters #-}

class FormatterBuilder d m where
    buildFormatter :: d -> Formatter m

class FormatterTBuilder d m where
    buildFormatterT :: d -> FormatterT m





data Styled s a = Styled a deriving (Show)
makeWrapped ''Styled

infixl 6 %
(%) :: forall a s. a -> s -> Styled s a
a % _ = Styled a

data Compact = Compact





instance {-# OVERLAPPABLE #-} (Pretty (LogData d), DataStore d m)
                 => FormatterBuilder d             m where buildFormatter _ = Formatter $ pretty <$> getData @d             ; {-# INLINE buildFormatter #-}
instance (Monad m, Pretty (Styled s (LogData d)), DataStore d m)
                 => FormatterBuilder (Styled s d)  m where buildFormatter _ = Formatter $ pretty . Styled @s <$> getData @d ; {-# INLINE buildFormatter #-}
instance (m ~ n) => FormatterBuilder (Formatter n) m where buildFormatter   = id                                            ; {-# INLINE buildFormatter #-}
instance Monad m => FormatterBuilder String        m where buildFormatter   = Formatter . return . text                     ; {-# INLINE buildFormatter #-}
instance Monad m => FormatterBuilder Doc           m where buildFormatter   = Formatter . return                            ; {-# INLINE buildFormatter #-}


instance {-# OVERLAPPABLE #-} (PrettyT (LogData d) m, DataStore d m)
                          => FormatterTBuilder d             m where buildFormatterT _ = FormatterT $ \d -> flip prettyT d =<< getData @d      ; {-# INLINE buildFormatterT #-}
instance {-# OVERLAPPABLE #-} (PrettyT (Styled s (LogData d)) m, DataStore d m)
                          => FormatterTBuilder (Styled s d)  m where buildFormatterT _ = FormatterT $ \d -> flip prettyT d . Styled @s =<< getData @d      ; {-# INLINE buildFormatterT #-}
instance (m ~ n, Monad m) => FormatterTBuilder (Formatter n) m where buildFormatterT a = FormatterT $ \d -> (<>) <$> runFormatter a <*> pure d ; {-# INLINE buildFormatterT #-}
instance         Monad m  => FormatterTBuilder String        m where buildFormatterT a = FormatterT $ return . (text a <>)                     ; {-# INLINE buildFormatterT #-}
instance         Monad m  => FormatterTBuilder Doc           m where buildFormatterT a = FormatterT $ return . (a <>)                          ; {-# INLINE buildFormatterT #-}


class PrettyT a m where
    prettyT :: a -> Doc -> m Doc

instance {-# OVERLAPPABLE #-} (Pretty a, Monad m) => PrettyT a m where
    prettyT a = return . (pretty a <>) ; {-# INLINE prettyT #-}


-- === Basic formatters === --
--
-- defaultFormatter :: DataStores '[Msg, Priority] m => Formatter m
-- defaultFormatter = colorLvlFormatter ("[" <:> Priority <:> "] ") <:> Msg <:> Doc.hardline ; {-# INLINE defaultFormatter #-}

defaultFormatter :: DataStores '[DynTags, Msg] m => Formatter m
defaultFormatter = ("[" <:> DynTags <:> "] ") <:> Msg <:> Doc.hardline ; {-# INLINE defaultFormatter #-}

nestedReportedFormatter :: DataStores '[DynTags, Nesting, Reporter, Msg] m => Formatter m
nestedReportedFormatter = ("[" <:> DynTags <:> "] ") <:> Nesting <:> Reporter <:> ": " <:> Msg <:> Doc.hardline ; {-# INLINE nestedReportedFormatter #-}
-- nestedReportedFormatter = ("[" <:> Priority % Compact <:> "] ") <:> Nesting <:> Reporter <:> ": " <:> Msg <:> Doc.hardline ; {-# INLINE nestedReportedFormatter #-}

-- defaultTimeFormatter :: DataStores '[Time, Loc, Priority, Msg] m => Formatter m
-- defaultTimeFormatter = ("[" <:> Priority % Compact <:> "] ") <:> Time <:> ": " <:> Loc <:> ": " <:> Msg <:> Doc.hardline ; {-# INLINE defaultTimeFormatter #-}

-- colorLvlFormatter :: DataStore Priority m => Formatter m -> Formatter m
-- colorLvlFormatter f = Formatter ((lvlColor . toEnum <$> getData' @Priority) <*> runFormatter f)


-- lvlColor :: LogLvl -> Doc -> Doc
-- lvlColor lvl
--     | lvl == Debug   = Doc.blue
--     | lvl <= Notice  = Doc.green
--     | lvl <= Warning = Doc.yellow
--     | otherwise = Doc.red
-- {-# INLINE lvlColor #-}



instance Pretty (LogData Nesting) where
    pretty (LogData n) = fold $ replicate (2 * n) Doc.space ; {-# INLINE pretty #-}

instance Pretty (LogData Msg) where
    pretty = unwrap' ; {-# INLINE pretty #-}

instance Pretty (LogData Reporter) where
    pretty = pretty . unwrap' ; {-# INLINE pretty #-}

-- instance Pretty (LogData Priority) where
--     pretty = text . show . toEnum @LogLvl . unwrap' ; {-# INLINE pretty #-}

-- instance Pretty (Styled Compact (LogData Priority)) where
--     pretty = text . pure . head . show . toEnum @LogLvl . unwrap' . unwrap' ; {-# INLINE pretty #-}

instance Pretty (LogData Time) where
    pretty = text . formatTime defaultTimeLocale "%c" . unwrap' ; {-# INLINE pretty #-}

instance Pretty (LogData Loc) where
    pretty (LogData loc) = text $ srcLocFile loc <> ":" <> show (srcLocStartLine loc) ; {-# INLINE pretty #-}

instance Pretty (LogData DynTags) where
    pretty = text . intercalate ", " . fmap (Typeable.tyConName . Typeable.typeRepTyCon) . convert . unwrap' ; {-# INLINE pretty #-}


instance Pretty Text where pretty = text . convert ; {-# INLINE pretty #-} -- FIXME: Text -> String -> Doc is not the optimal path.








-------------------------
-- === PlainLogger === --
-------------------------

data Plain
type PlainLogger = Logger Plain

newtype instance Logger Plain m a = PlainLogger { fromPlainLogger :: IdentityT m a}
        deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadTrans)

clearStyles :: DataStore Msg m => m ()
clearStyles = modifyData_ @Msg (wrapped %~ Doc.plain) ; {-# INLINE clearStyles #-}

plain :: PlainLogger m a -> m a
plain = runIdentityT . fromPlainLogger ; {-# INLINE plain #-}


-- === Instances === --

instance DataStore Msg m => IsLogger Plain m where
    runLogger = clearStyles ; {-# INLINE runLogger #-}



-- TODO
-- [x] Plain logger
-- [x] Logger tags
-- [x] Safe Levels declaration
-- [x] WriterLogger (getLogs, clearLogs, etc.)
-- [x] Loction logging
-- [ ] Distribute to modules
-- [ ] FilterLogger - filtering messages at ocmpile time without computing everything (!)
-- [ ] Threading loggers







-------------------
-- === Tests === --
-------------------

tst :: Logging m => m ()
tst = runIdentityT $ do
    -- withDebug "foo" $ do
    --     debug "bar"
    debug "baz"
    return ()


lmain :: IO ()
lmain = do
    -- dropLogs tst
    -- runTaggedLogging $ runEchoLogger $ runFormatLogger defaultFormatter $ tst
    runTaggedLogging $ runEchoLogger $ runFormatLogger defaultFormatter $ tst
    -- runTaggedLogging $ runPriorityLogger @StdLogLevels $ runEchoLogger $ runFormatLogger defaultFormatter $ tst
    -- let logs = runIdentity $ runPriorityLogging $ execWriterLogger @Msg $ tst
    -- print "---"
    -- print logs
    -- dropLogs tst

    -- runNestedLogger $ runStdLogger $ runEchoLogger tst
    -- dropLogs tst
    print "hello"
