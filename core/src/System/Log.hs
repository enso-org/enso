{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE NoOverloadedStrings  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs                #-}

module System.Log where


import           Prologue hiding (log, nested)
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


-- === DataStore === --

type DataStore' m = DataStore (RequiredData m)
data DataStore ds where
    Null :: DataStore '[]
    (:-:) :: LogData d -> DataStore ds -> DataStore (d ': ds)
infixr 5 :-:


-- === Data lookup === --

lookupData' :: forall d ds. LookupData d ds => DataStore ds -> Maybe (DataOf d)
lookupData' = fmap unwrap' . lookupData @d ; {-# INLINE lookupData' #-}

type family RequiredData (m :: * -> *) :: [*] where
    RequiredData (BaseLogger req _) = req
    RequiredData IO                 = '[]
    RequiredData (t m)              = RequiredData m

type        RequiredLookup  m d = LookupData d (RequiredData m)
type family RequiredLookups m ds :: Constraint where
    RequiredLookups m '[] = ()
    RequiredLookups m (d ': ds) = (RequiredLookup m d, RequiredLookups m ds)

class                        LookupData d ds        where lookupData :: DataStore ds -> Maybe (LogData d)
instance {-# OVERLAPPING #-} LookupData d (d ': ds) where lookupData (d :-: _ ) = Just d        ; {-# INLINE lookupData #-}
instance LookupData t ds  => LookupData t (d ': ds) where lookupData (_ :-: ds) = lookupData ds ; {-# INLINE lookupData #-}
instance                     LookupData d '[]       where lookupData _          = Nothing       ; {-# INLINE lookupData #-}



-- | BaseLogger is hack, which provides required data information.
--   We should discover it automatically, but there is no easy solution which doesn't involve
--   very complex TF transformations and DataStore transformations (which could eventually be optimized away).
------------------------
-- === BaseLogger === --
------------------------

data Base (req :: [*])
type BaseLogger req = Logger (Base req)
type instance LoggerDefinition (Base req) = IdentityT

runLogger :: forall req m a. Monad m => BaseLogger req m a -> m a
runLogger = runIdentityT . unwrap' ; {-# INLINE runLogger #-}



--------------------
-- === Logger === --
--------------------

type family LoggerDefinition l :: (* -> *) -> * -> *
newtype Logger l m a = Logger (LoggerDefinition l m a)
makeWrapped ''Logger

deriving instance Functor     (LoggerDefinition l m) => Functor     (Logger l m)
deriving instance Applicative (LoggerDefinition l m) => Applicative (Logger l m)
deriving instance Monad       (LoggerDefinition l m) => Monad       (Logger l m)
deriving instance MonadIO     (LoggerDefinition l m) => MonadIO     (Logger l m)
deriving instance MonadFix    (LoggerDefinition l m) => MonadFix    (Logger l m)
deriving instance MonadTrans  (LoggerDefinition l)   => MonadTrans  (Logger l)

instance (MonadTrans (Logger l), Monad (Logger l m), PrimMonad m)
      => PrimMonad (Logger l m) where
    type PrimState (Logger l m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}


-- === Logger classes === --

class IsLogger l m where
    submitLog :: DataStore' m -> Logger l m ()

-- | Passing log data to all found loggers down the monad transformers line
class    Monad m                                   => MonadLogger m            where passLog :: DataStore' m -> m ()
instance {-# OVERLAPPABLE #-} MonadLoggerFound l m => MonadLogger (Logger l m) where passLog d = submitLog d >> lift (passLog d) ; {-# INLINE passLog #-}
instance {-# OVERLAPPABLE #-} MonadLoggerTrans t m => MonadLogger (t m)        where passLog = lift . passLog                    ; {-# INLINE passLog #-}
type MonadLoggerFound l m = (MonadLoggerTrans (Logger l) m, IsLogger l m, MonadLogger m)
type MonadLoggerTrans t m = (Monad m, Monad (t m), MonadTrans t, MonadLogger m, RequiredData m ~ RequiredData (t m))

-- BaseLogger hack (read more in BaseLogger description)
instance Monad m => MonadLogger (BaseLogger req m) where
    passLog _ = return () ; {-# INLINE passLog #-}



---------------------------
-- === Data Provider === --
---------------------------

-- === Definition === --

newtype DataProvider d m a = DataProvider (StateT (LogData d) m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)
makeWrapped ''DataProvider


-- === Management === --

provideData :: forall d m a. Monad m => LogData d -> DataProvider d m a -> m a
provideData = flip (State.evalStateT . unwrap') ; {-# INLINE provideData #-}

provideData' :: forall d m a. (Monad m, Default (LogData d)) => DataProvider d m a -> m a
provideData' = provideData def ; {-# INLINE provideData' #-}

modifyDataM :: forall d a m. DataLogging d m => (LogData d -> m (a, LogData d)) -> m a
modifyDataM f = do
    d <- getData
    (a, d') <- f d
    putData d'
    return a
{-# INLINE modifyDataM #-}

modifyDataM_ :: forall d m. DataLogging d m => (LogData d -> m (LogData d)) -> m ()
modifyDataM_ = modifyDataM . (fmap.fmap) ((),) ; {-# INLINE modifyDataM_ #-}

modifyData :: forall d a m. DataLogging d m => (LogData d -> (a, LogData d)) -> m a
modifyData = modifyDataM . fmap return ; {-# INLINE modifyData #-}

modifyData_ :: forall d m. DataLogging d m => (LogData d -> LogData d) -> m ()
modifyData_ = modifyDataM_ . fmap return ; {-# INLINE modifyData_ #-}

withData :: forall d m a. DataLogging d m => LogData d -> m a -> m a
withData = withModData . const ; {-# INLINE withData #-}

withModData :: forall d m a. DataLogging d m => (LogData d -> LogData d) -> m a -> m a
withModData df f = do
    old <- getData @d
    putData $ df old
    out <- f
    putData old
    return out
{-# INLINE withModData #-}


-- === Data Logging === ---

class Monad m => DataLogging d m where
    getData :: m (LogData d)
    putData :: LogData d -> m ()

instance Monad m => DataLogging d (DataProvider d m) where
    getData = wrap'   State.get ; {-# INLINE getData #-}
    putData = wrap' . State.put ; {-# INLINE putData #-}

type DataLoggingTrans d t m = (Monad m, MonadTrans t, Monad (t m), DataLogging d m)
instance {-# OVERLAPPABLE #-} DataLoggingTrans d t m => DataLogging d (t m) where
    getData = lift   getData ; {-# INLINE getData #-}
    putData = lift . putData ; {-# INLINE putData #-}

type family DataSetLogging ds m :: Constraint where
    DataSetLogging '[] m = ()
    DataSetLogging (d ': ds) m = (DataLogging d m, DataSetLogging ds m)


-- === Data Gathering === --

type RequiredDataGather m = DataGather (RequiredData m) m
gatherRequiredData :: RequiredDataGather m => m (DataStore' m)
gatherRequiredData = gatherData ; {-# INLINE gatherRequiredData #-}


class    Monad m              => DataGather ds        m where gatherData :: m (DataStore ds)
instance Monad m              => DataGather '[]       m where gatherData = return Null                      ; {-# INLINE gatherData #-}
instance DataSubGather d ds m => DataGather (d ': ds) m where gatherData = (:-:) <$> getData <*> gatherData ; {-# INLINE gatherData #-}
type     DataSubGather d ds m = (DataGather ds m, DataLogging d m)


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

type DefaultLogging m = (RequiredDataGather m, MonadLogger m)
defaultLogging :: DefaultLogging m => m ()
defaultLogging = gatherRequiredData >>= passLog


-- === Instances === --

instance (Monad m, DefaultLogging (DataProvider d m))
      => MonadLogging (DataProvider d m) where
    log = defaultLogging ; {-# INLINE log #-}

instance (Monad m, Monad (Logger l m), DefaultLogging (Logger l m))
      => MonadLogging (Logger l m) where
    log = defaultLogging ; {-# INLINE log #-}

-- Standard monads
instance MonadLogging m => MonadLogging (StateT s m)
instance MonadLogging m => MonadLogging (IdentityT m)




--------------------------------------------------------
--------------------------------------------------------




-------------------------------------
-- === Standard logging levels === --
-------------------------------------

data StdLvl = Debug    -- Debug Logs
            | Info     -- Information
            | Notice   -- Normal runtime conditions
            | Warning  -- General Warnings
            | Error    -- General Errors
            | Critical -- Severe situations
            | Alert    -- Take immediate action
            | Panic    -- System is unusable
            deriving (Show, Ord, Eq, Enum)



------------------------------
-- === BaseLogger datas === --
------------------------------

-- === Msg === --

data Msg
type instance DataOf Msg = Text

msg :: ToText t => t -> LogData Msg
msg = wrap' . convert ; {-# INLINE msg #-}


-- === Reporter === --

data Reporter
type instance DataOf Reporter = Text

reporter :: ToText r => r -> LogData Reporter
reporter = wrap' . convert ; {-# INLINE reporter #-}

reported :: (DataLogging Reporter m, ToText r) => r -> m a -> m a
reported = withData . reporter ; {-# INLINE reported #-}


-- === Priority === --

data Priority
type instance DataOf Priority = Int

priority :: Enum p => p -> LogData Priority
priority = wrap' . fromEnum ; {-# INLINE priority #-}


-- === Nesting === --

data Nesting
type instance DataOf Nesting = Int

nesting :: Int -> LogData Nesting
nesting = wrap' ; {-# INLINE nesting #-}

withNesting :: DataLogging Nesting m => (LogData Nesting -> LogData Nesting) -> m ()
withNesting = modifyData_ @Nesting ; {-# INLINE withNesting #-}

incNesting :: DataLogging Nesting m => m ()
incNesting = withNesting succ ; {-# INLINE incNesting #-}

decNesting :: DataLogging Nesting m => m ()
decNesting = withNesting pred ; {-# INLINE decNesting #-}

nested :: DataLogging Nesting m => m a -> m a
nested = withModData @Nesting succ ; {-# INLINE nested #-}



------------------------------------
-- === Standard logging utils === --
------------------------------------

-- === Types === --

type MultiLogging ls m = (MonadLogging m, DataSetLogging ls    m)
type MsgLogging      m = (MonadLogging m, DataLogging Msg      m)
type PriorityLogging m = (MsgLogging   m, DataLogging Priority m)
type ReportedLogging m = (MsgLogging   m, DataLogging Reporter m)
type NestedLogging   m = (MsgLogging   m, DataLogging Nesting  m)

type Logging         m = (PriorityLogging m, ReportedLogging m, NestedLogging m)


-- === Generic logging === --

priLog :: (MultiLogging '[Priority, Msg] m, Enum priority, ToText msg)
       => priority -> msg -> m ()
priLog p m = withData (priority p) $ withData (msg m) log

priLogBy :: (MultiLogging '[Priority, Reporter, Msg] m, Enum priority, ToText reporter, ToText msg)
         => priority -> reporter -> msg -> m ()
priLogBy p r m = reported r $ priLog p m ; {-# INLINE priLogBy #-}

withPriLog :: (MultiLogging '[Nesting, Priority, Msg] m, Enum priority, ToText msg)
           => priority -> msg -> m a -> m a
withPriLog p m f = priLog p m >> nested f ; {-# INLINE withPriLog #-}

withPriLogBy :: (MultiLogging '[Nesting, Priority, Reporter, Msg] m, Enum priority, ToText reporter, ToText msg)
             => priority -> reporter -> msg -> m a -> m a
withPriLogBy p r m f = priLogBy p r m >> nested f ; {-# INLINE withPriLogBy #-}


-- === Loggign utils === --

debug, info, notice, warning, err, critical, alert, panic
    :: (MultiLogging '[Priority, Msg] m, ToText msg) => msg -> m ()
debug    = priLog Debug    ; {-# INLINE debug    #-}
info     = priLog Info     ; {-# INLINE info     #-}
notice   = priLog Notice   ; {-# INLINE notice   #-}
warning  = priLog Warning  ; {-# INLINE warning  #-}
err      = priLog Error    ; {-# INLINE err      #-}
critical = priLog Critical ; {-# INLINE critical #-}
alert    = priLog Alert    ; {-# INLINE alert    #-}
panic    = priLog Panic    ; {-# INLINE panic    #-}

debugBy, infoBy, noticeBy, warningBy, errBy, criticalBy, alertBy, panicBy
    :: (Logging m, ToText reporter, ToText msg) => reporter -> msg -> m ()
debugBy    = priLogBy Debug    ; {-# INLINE debugBy    #-}
infoBy     = priLogBy Info     ; {-# INLINE infoBy     #-}
noticeBy   = priLogBy Notice   ; {-# INLINE noticeBy   #-}
warningBy  = priLogBy Warning  ; {-# INLINE warningBy  #-}
errBy      = priLogBy Error    ; {-# INLINE errBy      #-}
criticalBy = priLogBy Critical ; {-# INLINE criticalBy #-}
alertBy    = priLogBy Alert    ; {-# INLINE alertBy    #-}
panicBy    = priLogBy Panic    ; {-# INLINE panicBy    #-}

withDebug, withInfo, withNotice, withWarning, withError, withCritical, withAlert, withPanic
    :: (Logging m, ToText msg) => msg -> m a -> m a
withDebug    = withPriLog Debug    ; {-# INLINE withDebug    #-}
withInfo     = withPriLog Info     ; {-# INLINE withInfo     #-}
withNotice   = withPriLog Notice   ; {-# INLINE withNotice   #-}
withWarning  = withPriLog Warning  ; {-# INLINE withWarning  #-}
withError    = withPriLog Error    ; {-# INLINE withError    #-}
withCritical = withPriLog Critical ; {-# INLINE withCritical #-}
withAlert    = withPriLog Alert    ; {-# INLINE withAlert    #-}
withPanic    = withPriLog Panic    ; {-# INLINE withPanic    #-}

withDebugBy, withInfoBy, withNoticeBy, withWarningBy, withErrorBy, withCriticalBy, withAlertBy, withPanicBy
    :: (Logging m, ToText reporter, ToText msg) => reporter -> msg -> m a -> m a
withDebugBy    = withPriLogBy Debug    ; {-# INLINE withDebugBy    #-}
withInfoBy     = withPriLogBy Info     ; {-# INLINE withInfoBy     #-}
withNoticeBy   = withPriLogBy Notice   ; {-# INLINE withNoticeBy   #-}
withWarningBy  = withPriLogBy Warning  ; {-# INLINE withWarningBy  #-}
withErrorBy    = withPriLogBy Error    ; {-# INLINE withErrorBy    #-}
withCriticalBy = withPriLogBy Critical ; {-# INLINE withCriticalBy #-}
withAlertBy    = withPriLogBy Alert    ; {-# INLINE withAlertBy    #-}
withPanicBy    = withPriLogBy Panic    ; {-# INLINE withPanicBy    #-}


-- === Running utils === --

runStdLogger :: forall m a req. (Monad m, req ~ '[Priority, Nesting, Msg]) => BaseLogger req m a -> m a
runStdLogger = runLogger @req ; {-# INLINE runStdLogger #-}

runLogging :: forall req m a. (Monad m, req ~ '[Priority, Nesting, Reporter, Msg])
           => DataProvider Msg
            ( DataProvider Reporter
            $ DataProvider Nesting
            $ DataProvider Priority
            $ BaseLogger req m
            ) a -> m a
runLogging = runLogger @req
           . provideData (priority Debug)
           . provideData' @Nesting
           . provideData (reporter "")
           . provideData (msg "")
{-# INLINE runLogging #-}






--------------------------------------------------------
-- Loggers
--------------------------------------------------------





------------------------
-- === EchoLogger === --
------------------------

data Echo
type instance LoggerDefinition Echo = IdentityT
type EchoLogger = Logger Echo

runEchoLogger :: EchoLogger m a -> m a
runEchoLogger = runIdentityT . unwrap' ; {-# INLINE runEchoLogger #-}


-- === Instances === --

instance (MonadIO m, RequiredLookups m '[Msg, Nesting]) => IsLogger Echo m where
    submitLog ds = putStrLn (ident <> msg) where
        space = replicate 2 ' '
        ident = concat $ replicate nest space
        msg   = convert $ fromJust $ lookupData' @Msg     ds -- FIXME [WD]: unsafe
        nest  =           fromJust $ lookupData' @Nesting ds -- FIXME [WD]: unsafe
        fromJust (Just a) = a



------------------------
-- === DropLogger === --
------------------------

data Drop
type instance LoggerDefinition Drop = IdentityT
type DropLogger = Logger Drop

dropLogs :: DropLogger m a -> m a
dropLogs = runIdentityT . unwrap' ; {-# INLINE dropLogs #-}


-- === Instances === --

instance {-# OVERLAPPING #-}
         Monad   m => MonadLogging (DropLogger m) where log       = return () ; {-# INLINE log     #-}
instance MonadIO m => MonadLogger  (DropLogger m) where passLog _ = return () ; {-# INLINE passLog #-}

-- This is hacky, but because Drop logger is guaranteed to drop all logs,
-- we can be sure the information will not be needed.
instance Monad m => DataLogging d (DropLogger m) where
    getData   = return (error "impossible") ; {-# INLINE getData #-}
    putData _ = return (error "impossible") ; {-# INLINE putData #-}















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
    runLogging $ runEchoLogger $ dropLogs tst
    -- dropLogs tst

    -- runNestedLogger $ runStdLogger $ runEchoLogger tst2
    -- dropLogs tst
    print "hello"
