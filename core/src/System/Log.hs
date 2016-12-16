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

import GHC.Stack


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




-------------------------------------
-- === Standard logging levels === --
-------------------------------------

data LogLvl = Debug    -- Debug Logs
            | Info     -- Information
            | Notice   -- Normal runtime conditions
            | Warning  -- General Warnings
            | Error    -- General Errors
            | Critical -- Severe situations
            | Alert    -- Take immediate action
            | Panic    -- System is unusable
            deriving (Show, Ord, Eq, Enum)

type IsDoc t = Convertible t Doc -- FIXME: refactor

instance Convertible String Doc where
    convert s = Doc.text s ; {-# INLINE convert #-}


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

type Logging         m = (LocLogging m, PriorityLogging m, ReportedLogging m, NestedLogging m)


-- === Generic logging === --

addCallStackInfo :: (HasCallStack, DataStore Loc m) => m a -> m a
addCallStackInfo = withData (loc . snd . head . drop 1 $ getCallStack callStack)

priLog' :: (HasCallStack, MultiLogging '[Loc, Priority, Msg] m, Enum priority, IsDoc msg)
       => priority -> msg -> m ()
priLog' p m = withData (priority p) $ withData (msg m) log

priLogBy' :: (HasCallStack, MultiLogging '[Loc, Priority, Reporter, Msg] m, Enum priority, ToText reporter, IsDoc msg)
         => priority -> reporter -> msg -> m ()
priLogBy' p r m = reported r $ priLog' p m ; {-# INLINE priLogBy' #-}

withPriLog' :: (HasCallStack, MultiLogging '[Loc, Nesting, Priority, Msg] m, Enum priority, IsDoc msg)
           => priority -> msg -> m a -> m a
withPriLog' p m f = priLog' p m >> nested f ; {-# INLINE withPriLog' #-}

withPriLogBy' :: (HasCallStack, MultiLogging '[Loc, Nesting, Priority, Reporter, Msg] m, Enum priority, ToText reporter, IsDoc msg)
             => priority -> reporter -> msg -> m a -> m a
withPriLogBy' p r m f = priLogBy' p r m >> nested f ; {-# INLINE withPriLogBy' #-}


-- === Loggign utils === --

debug, info, notice, warning, err, critical, alert, panic
    :: (HasCallStack, MultiLogging '[Loc, Priority, Msg] m, IsDoc msg)
    => msg -> m ()
debug    = addCallStackInfo . priLog' Debug    ; {-# INLINE debug    #-}
info     = addCallStackInfo . priLog' Info     ; {-# INLINE info     #-}
notice   = addCallStackInfo . priLog' Notice   ; {-# INLINE notice   #-}
warning  = addCallStackInfo . priLog' Warning  ; {-# INLINE warning  #-}
err      = addCallStackInfo . priLog' Error    ; {-# INLINE err      #-}
critical = addCallStackInfo . priLog' Critical ; {-# INLINE critical #-}
alert    = addCallStackInfo . priLog' Alert    ; {-# INLINE alert    #-}
panic    = addCallStackInfo . priLog' Panic    ; {-# INLINE panic    #-}

debugBy, infoBy, noticeBy, warningBy, errBy, criticalBy, alertBy, panicBy
    :: (HasCallStack, MultiLogging '[Loc, Priority, Reporter, Msg] m, ToText reporter, IsDoc msg)
    => reporter -> msg -> m ()
debugBy    = addCallStackInfo .: priLogBy' Debug    ; {-# INLINE debugBy    #-}
infoBy     = addCallStackInfo .: priLogBy' Info     ; {-# INLINE infoBy     #-}
noticeBy   = addCallStackInfo .: priLogBy' Notice   ; {-# INLINE noticeBy   #-}
warningBy  = addCallStackInfo .: priLogBy' Warning  ; {-# INLINE warningBy  #-}
errBy      = addCallStackInfo .: priLogBy' Error    ; {-# INLINE errBy      #-}
criticalBy = addCallStackInfo .: priLogBy' Critical ; {-# INLINE criticalBy #-}
alertBy    = addCallStackInfo .: priLogBy' Alert    ; {-# INLINE alertBy    #-}
panicBy    = addCallStackInfo .: priLogBy' Panic    ; {-# INLINE panicBy    #-}

withDebug, withInfo, withNotice, withWarning, withError, withCritical, withAlert, withPanic
    :: (HasCallStack, MultiLogging '[Loc, Nesting, Priority, Msg] m, IsDoc msg)
    => msg -> m a -> m a
withDebug    = addCallStackInfo .: withPriLog' Debug    ; {-# INLINE withDebug    #-}
withInfo     = addCallStackInfo .: withPriLog' Info     ; {-# INLINE withInfo     #-}
withNotice   = addCallStackInfo .: withPriLog' Notice   ; {-# INLINE withNotice   #-}
withWarning  = addCallStackInfo .: withPriLog' Warning  ; {-# INLINE withWarning  #-}
withError    = addCallStackInfo .: withPriLog' Error    ; {-# INLINE withError    #-}
withCritical = addCallStackInfo .: withPriLog' Critical ; {-# INLINE withCritical #-}
withAlert    = addCallStackInfo .: withPriLog' Alert    ; {-# INLINE withAlert    #-}
withPanic    = addCallStackInfo .: withPriLog' Panic    ; {-# INLINE withPanic    #-}

withDebugBy, withInfoBy, withNoticeBy, withWarningBy, withErrorBy, withCriticalBy, withAlertBy, withPanicBy
    :: (HasCallStack, MultiLogging '[Loc, Nesting, Priority, Reporter, Msg] m, ToText reporter, IsDoc msg)
    => reporter -> msg -> m a -> m a
withDebugBy    = addCallStackInfo .:. withPriLogBy' Debug    ; {-# INLINE withDebugBy    #-}
withInfoBy     = addCallStackInfo .:. withPriLogBy' Info     ; {-# INLINE withInfoBy     #-}
withNoticeBy   = addCallStackInfo .:. withPriLogBy' Notice   ; {-# INLINE withNoticeBy   #-}
withWarningBy  = addCallStackInfo .:. withPriLogBy' Warning  ; {-# INLINE withWarningBy  #-}
withErrorBy    = addCallStackInfo .:. withPriLogBy' Error    ; {-# INLINE withErrorBy    #-}
withCriticalBy = addCallStackInfo .:. withPriLogBy' Critical ; {-# INLINE withCriticalBy #-}
withAlertBy    = addCallStackInfo .:. withPriLogBy' Alert    ; {-# INLINE withAlertBy    #-}
withPanicBy    = addCallStackInfo .:. withPriLogBy' Panic    ; {-# INLINE withPanicBy    #-}


-- === Running utils === --

runLogging :: forall req m a. Monad m
           => DataProvider Loc
            ( DataProvider Msg
            $ DataProvider Reporter
            $ DataProvider Nesting
            $ DataProvider Priority m
            ) a -> m a
runLogging = provideData (priority Debug)
           . provideData' @Nesting
           . provideData (reporter "")
           . provideData (msg "")
           . provideData (loc $ error "No source location provided")
{-# INLINE runLogging #-}




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

defaultFormatter :: DataStores '[Msg, Priority] m => Formatter m
defaultFormatter = colorLvlFormatter ("[" <:> Priority <:> "] ") <:> Msg <:> Doc.hardline ; {-# INLINE defaultFormatter #-}

nestedReportedFormatter :: DataStores '[Priority, Nesting, Reporter, Msg] m => Formatter m
nestedReportedFormatter = colorLvlFormatter ("[" <:> Priority % Compact <:> "] ") <:> Nesting <:> Reporter <:> ": " <:> Msg <:> Doc.hardline ; {-# INLINE nestedReportedFormatter #-}

defaultTimeFormatter :: DataStores '[Time, Loc, Priority, Msg] m => Formatter m
defaultTimeFormatter = colorLvlFormatter ("[" <:> Priority % Compact <:> "] ") <:> Time <:> ": " <:> Loc <:> ": " <:> Msg <:> Doc.hardline ; {-# INLINE defaultTimeFormatter #-}

colorLvlFormatter :: DataStore Priority m => Formatter m -> Formatter m
colorLvlFormatter f = Formatter ((lvlColor . toEnum <$> getData' @Priority) <*> runFormatter f)


lvlColor :: LogLvl -> Doc -> Doc
lvlColor lvl
    | lvl == Debug   = Doc.blue
    | lvl <= Notice  = Doc.green
    | lvl <= Warning = Doc.yellow
    | otherwise = Doc.red
{-# INLINE lvlColor #-}



instance Pretty (LogData Nesting) where
    pretty (LogData n) = fold $ replicate (2 * n) Doc.space ; {-# INLINE pretty #-}

instance Pretty (LogData Msg) where
    pretty = unwrap' ; {-# INLINE pretty #-}

instance Pretty (LogData Reporter) where
    pretty = pretty . unwrap' ; {-# INLINE pretty #-}

instance Pretty (LogData Priority) where
    pretty = text . show . toEnum @LogLvl . unwrap' ; {-# INLINE pretty #-}

instance Pretty (Styled Compact (LogData Priority)) where
    pretty = text . pure . head . show . toEnum @LogLvl . unwrap' . unwrap' ; {-# INLINE pretty #-}

instance Pretty (LogData Time) where
    pretty = text . formatTime defaultTimeLocale "%c" . unwrap' ; {-# INLINE pretty #-}

instance Pretty (LogData Loc) where
    pretty (LogData loc) = text $ srcLocFile loc <> ":" <> show (srcLocStartLine loc) ; {-# INLINE pretty #-}

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
-- [ ] Logger tags
-- [ ] Safe Levels declaration
-- [ ] Distribute to modules
-- [x] WriterLogger (getLogs, clearLogs, etc.)
-- [x] Loction logging
-- [ ] FilterLogger - filtering messages at ocmpile time without computing everything (!)
-- [ ] Threading loggers







-------------------
-- === Tests === --
-------------------

tst :: Logging m => m ()
tst = runIdentityT $ do
    withDebug "foo" $ do
        debug "bar"
    debug "baz"
    return ()


lmain :: IO ()
lmain = do
    dropLogs tst
    runLogging $ runEchoLogger $ runFormatLogger defaultTimeFormatter $ tst
    let logs = runIdentity $ runLogging $ execWriterLogger @Msg $ tst
    print "---"
    print logs
    -- dropLogs tst

    -- runNestedLogger $ runStdLogger $ runEchoLogger tst2
    -- dropLogs tst
    print "hello"
