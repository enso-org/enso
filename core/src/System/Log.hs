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

import Data.Time.Clock              (UTCTime)
import Data.Time.Format             (formatTime)
import Data.Time.Locale.Compat      (defaultTimeLocale)
import Text.PrettyPrint.ANSI.Leijen (Doc, Pretty, text, pretty, putDoc)
import qualified Text.PrettyPrint.ANSI.Leijen as Doc



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
type MsgLogging      m = (MonadLogging m, DataStore Msg      m)
type PriorityLogging m = (MsgLogging   m, DataStore Priority m)
type ReportedLogging m = (MsgLogging   m, DataStore Reporter m)
type NestedLogging   m = (MsgLogging   m, DataStore Nesting  m)

type Logging         m = (PriorityLogging m, ReportedLogging m, NestedLogging m)


-- === Generic logging === --

priLog :: (MultiLogging '[Priority, Msg] m, Enum priority, IsDoc msg)
       => priority -> msg -> m ()
priLog p m = withData (priority p) $ withData (msg m) log

priLogBy :: (MultiLogging '[Priority, Reporter, Msg] m, Enum priority, ToText reporter, IsDoc msg)
         => priority -> reporter -> msg -> m ()
priLogBy p r m = reported r $ priLog p m ; {-# INLINE priLogBy #-}

withPriLog :: (MultiLogging '[Nesting, Priority, Msg] m, Enum priority, IsDoc msg)
           => priority -> msg -> m a -> m a
withPriLog p m f = priLog p m >> nested f ; {-# INLINE withPriLog #-}

withPriLogBy :: (MultiLogging '[Nesting, Priority, Reporter, Msg] m, Enum priority, ToText reporter, IsDoc msg)
             => priority -> reporter -> msg -> m a -> m a
withPriLogBy p r m f = priLogBy p r m >> nested f ; {-# INLINE withPriLogBy #-}


-- === Loggign utils === --

debug, info, notice, warning, err, critical, alert, panic
    :: (MultiLogging '[Priority, Msg] m, IsDoc msg) => msg -> m ()
debug    = priLog Debug    ; {-# INLINE debug    #-}
info     = priLog Info     ; {-# INLINE info     #-}
notice   = priLog Notice   ; {-# INLINE notice   #-}
warning  = priLog Warning  ; {-# INLINE warning  #-}
err      = priLog Error    ; {-# INLINE err      #-}
critical = priLog Critical ; {-# INLINE critical #-}
alert    = priLog Alert    ; {-# INLINE alert    #-}
panic    = priLog Panic    ; {-# INLINE panic    #-}

debugBy, infoBy, noticeBy, warningBy, errBy, criticalBy, alertBy, panicBy
    :: (Logging m, ToText reporter, IsDoc msg) => reporter -> msg -> m ()
debugBy    = priLogBy Debug    ; {-# INLINE debugBy    #-}
infoBy     = priLogBy Info     ; {-# INLINE infoBy     #-}
noticeBy   = priLogBy Notice   ; {-# INLINE noticeBy   #-}
warningBy  = priLogBy Warning  ; {-# INLINE warningBy  #-}
errBy      = priLogBy Error    ; {-# INLINE errBy      #-}
criticalBy = priLogBy Critical ; {-# INLINE criticalBy #-}
alertBy    = priLogBy Alert    ; {-# INLINE alertBy    #-}
panicBy    = priLogBy Panic    ; {-# INLINE panicBy    #-}

withDebug, withInfo, withNotice, withWarning, withError, withCritical, withAlert, withPanic
    :: (Logging m, IsDoc msg) => msg -> m a -> m a
withDebug    = withPriLog Debug    ; {-# INLINE withDebug    #-}
withInfo     = withPriLog Info     ; {-# INLINE withInfo     #-}
withNotice   = withPriLog Notice   ; {-# INLINE withNotice   #-}
withWarning  = withPriLog Warning  ; {-# INLINE withWarning  #-}
withError    = withPriLog Error    ; {-# INLINE withError    #-}
withCritical = withPriLog Critical ; {-# INLINE withCritical #-}
withAlert    = withPriLog Alert    ; {-# INLINE withAlert    #-}
withPanic    = withPriLog Panic    ; {-# INLINE withPanic    #-}

withDebugBy, withInfoBy, withNoticeBy, withWarningBy, withErrorBy, withCriticalBy, withAlertBy, withPanicBy
    :: (Logging m, ToText reporter, IsDoc msg) => reporter -> msg -> m a -> m a
withDebugBy    = withPriLogBy Debug    ; {-# INLINE withDebugBy    #-}
withInfoBy     = withPriLogBy Info     ; {-# INLINE withInfoBy     #-}
withNoticeBy   = withPriLogBy Notice   ; {-# INLINE withNoticeBy   #-}
withWarningBy  = withPriLogBy Warning  ; {-# INLINE withWarningBy  #-}
withErrorBy    = withPriLogBy Error    ; {-# INLINE withErrorBy    #-}
withCriticalBy = withPriLogBy Critical ; {-# INLINE withCriticalBy #-}
withAlertBy    = withPriLogBy Alert    ; {-# INLINE withAlertBy    #-}
withPanicBy    = withPriLogBy Panic    ; {-# INLINE withPanicBy    #-}


-- === Running utils === --

runLogging :: forall req m a. Monad m
           => DataProvider Msg
            ( DataProvider Reporter
            $ DataProvider Nesting
            $ DataProvider Priority m
            ) a -> m a
runLogging = provideData (priority Debug)
           . provideData' @Nesting
           . provideData (reporter "")
           . provideData (msg "")
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
         Monad   m => MonadLogging  (DropLogger m) where log         = return () ; {-# INLINE log         #-}
instance MonadIO m => LoggersFinder (DropLogger m) where findLoggers = return () ; {-# INLINE findLoggers #-}

-- This is hacky, but because Drop logger is guaranteed to drop all logs,
-- we can be sure the information will not be needed.
instance Monad m => DataStore d (DropLogger m) where
    getData   = return (error "impossible") ; {-# INLINE getData #-}
    putData _ = return (error "impossible") ; {-# INLINE putData #-}






-----------------------------
-- === FormatLogger === --
-----------------------------

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



instance {-# OVERLAPPABLE #-} (Pretty (LogData d), DataStore d m)
                 => FormatterBuilder d             m where buildFormatter _ = Formatter $ pretty <$> getData @d ; {-# INLINE buildFormatter #-}
instance (m ~ n) => FormatterBuilder (Formatter n) m where buildFormatter   = id                                ; {-# INLINE buildFormatter #-}
instance Monad m => FormatterBuilder String        m where buildFormatter   = Formatter . return . text         ; {-# INLINE buildFormatter #-}
instance Monad m => FormatterBuilder Doc           m where buildFormatter   = Formatter . return                ; {-# INLINE buildFormatter #-}


instance {-# OVERLAPPABLE #-} (PrettyT (LogData d) m, DataStore d m)
                          => FormatterTBuilder d             m where buildFormatterT _ = FormatterT $ \d -> flip prettyT d =<< getData @d      ; {-# INLINE buildFormatterT #-}
instance (m ~ n, Monad m) => FormatterTBuilder (Formatter n) m where buildFormatterT a = FormatterT $ \d -> (<>) <$> runFormatter a <*> pure d ; {-# INLINE buildFormatterT #-}
instance         Monad m  => FormatterTBuilder String        m where buildFormatterT a = FormatterT $ return . (text a <>)                     ; {-# INLINE buildFormatterT #-}
instance         Monad m  => FormatterTBuilder Doc           m where buildFormatterT a = FormatterT $ return . (a <>)                          ; {-# INLINE buildFormatterT #-}


class PrettyT a m where
    prettyT :: a -> Doc -> m Doc

instance {-# OVERLAPPABLE #-} (Pretty a, Monad m) => PrettyT a m where
    prettyT a = return . (pretty a <>) ; {-# INLINE prettyT #-}


-- === Basic formatters === --

defaultFormatter :: DataStores '[Msg, Priority] m => Formatter m
defaultFormatter = "[" <:> Priority <:> "] " <:> Msg <:> Doc.hardline ; {-# INLINE defaultFormatter #-}

nestedReportedFormatter :: DataStores '[Nesting, Reporter, Msg] m => Formatter m
nestedReportedFormatter = Nesting <:> Reporter <:> ": " <:> Msg <:> Doc.hardline ; {-# INLINE nestedReportedFormatter #-}

-- defaultTimeFormatter = colorLvlFormatter ("[" <:> Lvl <:> "] ") <:> Time <:> ": " <:> Msg
-- defaultFormatterTH   = colorLvlFormatter ("[" <:> Lvl <:> "] ") <:> Loc <:> ": " <:> Msg

-- Color formatter



-- colorLvlFormatter f = Formatter (\s -> let (LevelData pr _) = readData Lvl s in lvlColor pr $ runFormatter f s)


lvlColor :: LogLvl -> Doc -> Doc
lvlColor lvl
    | lvl == Debug   = id
    | lvl <= Notice  = Doc.green
    | lvl <= Warning = Doc.yellow
    | otherwise = Doc.red
{-# INLINE lvlColor #-}


-- deriving instance Pretty (DataOf d) => Pretty (LogData d)
--
-- instance PPrint String where
--     pprint = text
--
-- instance Pretty a => PPrint a where
--     pprint = pretty
--
-- instance Pretty LevelData where
--     pretty (LevelData _ name) = text name
--
-- instance Pretty LocData where
--     pretty (LocData _ _ m (l,_) _) = text (m ++ ":" ++ show l)
--
-- instance Pretty UTCTime where
--     pretty = text . formatTime defaultTimeLocale "%c"

-- instance Monad m => PrettyT (LogData Nesting) m where
--     prettyT (LogData n) = return . Doc.indent (2 * n) ; {-# INLINE prettyT #-}

instance Pretty (LogData Nesting) where
    pretty (LogData n) = fold $ replicate (2 * n) Doc.space ; {-# INLINE pretty #-}

instance Pretty (LogData Msg) where
    pretty = unwrap' ; {-# INLINE pretty #-}

instance Pretty (LogData Reporter) where
    pretty = pretty . unwrap' ; {-# INLINE pretty #-}

instance Pretty (LogData Priority) where
    pretty = text . show . toEnum @LogLvl . unwrap' ; {-# INLINE pretty #-}



instance Pretty Text where pretty = text . convert ; {-# INLINE pretty #-} -- FIXME: Text -> String -> Doc is not the optimal path.
-- data Formatter
-- type instance LoggerDefinition Formatter = IdentityT
-- type FormatterLogger = Logger Formatter
--
-- dropLogs :: DropLogger m a -> m a
-- dropLogs = runIdentityT . unwrap' ; {-# INLINE dropLogs #-}
--
--
-- -- === Instances === --
--
-- instance {-# OVERLAPPING #-}
--          Monad   m => MonadLogging (DropLogger m) where log       = return () ; {-# INLINE log     #-}
-- instance MonadIO m => LoggersFinder  (DropLogger m) where findLoggers _ = return () ; {-# INLINE findLoggers #-}
--
-- -- This is hacky, but because Drop logger is guaranteed to drop all logs,
-- -- we can be sure the information will not be needed.
-- instance Monad m => DataStore d (DropLogger m) where
--     getData   = return (error "impossible") ; {-# INLINE getData #-}
--     putData _ = return (error "impossible") ; {-# INLINE putData #-}
















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
    -- runLogging $ runEchoLogger $ runFormatLogger defaultFormatter $ tst
    runLogging $ runEchoLogger $ tst
    -- dropLogs tst

    -- runNestedLogger $ runStdLogger $ runEchoLogger tst2
    -- dropLogs tst
    print "hello"
