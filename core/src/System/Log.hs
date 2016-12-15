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














-- === BaseLogger Data === --

type family DataOf  a

newtype LogData d = LogData (DataOf d)
makeWrapped ''LogData

instance Default (DataOf d) => Default (LogData d) where
    def = wrap' def ; {-# INLINE def #-}

deriving instance Eq   (DataOf a) => Eq   (LogData a)
deriving instance Ord  (DataOf a) => Ord  (LogData a)
deriving instance Enum (DataOf a) => Enum (LogData a)
deriving instance Show (DataOf a) => Show (LogData a)


----------------------------
-- === NestedLogger === --
----------------------------

-- newtype NestedLogger m a = NestedLogger (StateT Int m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)
-- makeWrapped ''NestedLogger
--
-- class Monad m => NestedLogging m where
--     getNesting :: m Int
--     putNesting :: Int -> m ()
--
-- instance Monad m => NestedLogging (NestedLogger m) where
--     getNesting = wrap' State.get   ; {-# INLINE getNesting #-}
--     putNesting = wrap' . State.put ; {-# INLINE putNesting #-}
--
-- type NestedLoggingTrans t m = (Monad m, Monad (t m), MonadTrans t, NestedLogging m)
-- instance {-# OVERLAPPABLE #-} NestedLoggingTrans t m => NestedLogging (t m) where
--     getNesting = lift getNesting   ; {-# INLINE getNesting #-}
--     putNesting = lift . putNesting ; {-# INLINE putNesting #-}
--
--
-- runNestedLogger :: Monad m => NestedLogger m a -> m a
-- runNestedLogger = flip State.evalStateT def . unwrap' ; {-# INLINE runNestedLogger #-}
--
-- modifyNesting :: NestedLogging m => (Int -> Int) -> m ()
-- modifyNesting f = do
--     n <- getNesting
--     putNesting $ f n
-- {-# INLINE modifyNesting #-}
--
-- incNesting :: NestedLogging m => m ()
-- incNesting = modifyNesting succ ; {-# INLINE incNesting #-}
--
-- decNesting :: NestedLogging m => m ()
-- decNesting = modifyNesting pred ; {-# INLINE decNesting #-}
--
-- nested :: NestedLogging m => m a -> m a
-- nested f = do
--     incNesting
--     out <- f
--     decNesting
--     return out
-- {-# INLINE nested #-}

-- === Instances === --

-- instance PrimMonad m => PrimMonad (NestedLogger m) where
--     type PrimState (NestedLogger m) = PrimState m
--     primitive = lift . primitive ; {-# INLINE primitive #-}
--

--------------------
-- === Logger === --
--------------------

type family LoggerDefinition l :: (* -> *) -> * -> *
newtype Logger (req :: [*]) l m a = Logger (LoggerDefinition l m a)
makeWrapped ''Logger

deriving instance Functor     (LoggerDefinition l m) => Functor     (Logger req l m)
deriving instance Applicative (LoggerDefinition l m) => Applicative (Logger req l m)
deriving instance Monad       (LoggerDefinition l m) => Monad       (Logger req l m)
deriving instance MonadIO     (LoggerDefinition l m) => MonadIO     (Logger req l m)
deriving instance MonadFix    (LoggerDefinition l m) => MonadFix    (Logger req l m)
deriving instance MonadTrans  (LoggerDefinition l)   => MonadTrans  (Logger req l)


------------------------
-- === BaseLogger === --
------------------------

-- === Definition === --

newtype BaseLogger (req :: [*]) m a = BaseLogger (IdentityT m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)
makeWrapped ''BaseLogger


-- === Instances === --

instance PrimMonad m => PrimMonad (BaseLogger req m) where
    type PrimState (BaseLogger req m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}


-- === Type Dependencies === --

type DataStore' m = DataStore (RequiredData m)
data DataStore ds where
    Null :: DataStore '[]
    (:-:) :: LogData d -> DataStore ds -> DataStore (d ': ds)
infixr 5 :-:

type family RequiredData (m :: * -> *) :: [*] where
    RequiredData (BaseLogger req _) = req
    RequiredData IO                 = '[]
    RequiredData (t m)              = RequiredData m

lookupData' :: forall d ds. LookupData d ds => DataStore ds -> Maybe (DataOf d)
lookupData' = fmap unwrap' . lookupData @d ; {-# INLINE lookupData' #-}

type RequiredLookup m d = LookupData d (RequiredData m)
type family RequiredLookups m ds :: Constraint where
    RequiredLookups m '[] = ()
    RequiredLookups m (d ': ds) = (RequiredLookup m d, RequiredLookups m ds)

class                        LookupData d ds        where lookupData :: DataStore ds -> Maybe (LogData d)
instance {-# OVERLAPPING #-} LookupData d (d ': ds) where lookupData (d :-: _ ) = Just d        ; {-# INLINE lookupData #-}
instance LookupData t ds  => LookupData t (d ': ds) where lookupData (_ :-: ds) = lookupData ds ; {-# INLINE lookupData #-}
instance                     LookupData d '[]       where lookupData _          = Nothing       ; {-# INLINE lookupData #-}


-- === MonadLogger === --

class    Monad m                                   => MonadLogger m                  where submitLog :: DataStore' m -> m ()
instance {-# OVERLAPPABLE #-} MonadLoggerTrans t m => MonadLogger (t m)              where submitLog = lift . submitLog ; {-# INLINE submitLog #-}
type MonadLoggerTrans t m = (Monad m, Monad (t m), MonadTrans t, MonadLogger m, RequiredData m ~ RequiredData (t m))

instance MonadLogger m => MonadLogger (DataProvider d m) where submitLog = lift . submitLog ; {-# INLINE submitLog #-}
instance MonadLogger m => MonadLogger (IdentityT      m) where submitLog = lift . submitLog ; {-# INLINE submitLog #-}
instance MonadLogger m => MonadLogger (StateT       s m) where submitLog = lift . submitLog ; {-# INLINE submitLog #-}


instance Monad m => MonadLogger (BaseLogger req m) where submitLog _ = return () ; {-# INLINE submitLog #-}

-- === Running === --

runLogger :: forall req m a. Monad m => BaseLogger req m a -> m a
runLogger = runIdentityT . unwrap' ; {-# INLINE runLogger #-}


-- === Data Provider === --

newtype DataProvider d m a = DataProvider (StateT (LogData d) m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)
makeWrapped ''DataProvider

provideData :: forall d m a. Monad m => LogData d -> DataProvider d m a -> m a
provideData = flip (State.evalStateT . unwrap') ; {-# INLINE provideData #-}

provideData' :: forall d m a. (Monad m, Default (LogData d)) => DataProvider d m a -> m a
provideData' = provideData def ; {-# INLINE provideData' #-}


type family DataSetLogging ds m :: Constraint where
    DataSetLogging '[] m = ()
    DataSetLogging (d ': ds) m = (DataLogging d m, DataSetLogging ds m)

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


-- === Instances === --

instance PrimMonad m => PrimMonad (DataProvider d m) where
    type PrimState (DataProvider d m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}


















instance {-# OVERLAPPABLE #-}
         (Monad m, Default (LogData d)) => DefaultData m d where defaultData = return def
class     Monad m                       => DefaultData m d where defaultData :: m (LogData d)



type RequiredDataGather m = DataGather (RequiredData m) m
gatherRequiredData :: RequiredDataGather m => m (DataStore' m)
gatherRequiredData = gatherData ; {-# INLINE gatherRequiredData #-}


class    Monad m              => DataGather ds        m where gatherData :: m (DataStore ds)
instance Monad m              => DataGather '[]       m where gatherData = return Null                      ; {-# INLINE gatherData #-}
instance DataSubGather d ds m => DataGather (d ': ds) m where gatherData = (:-:) <$> getData <*> gatherData ; {-# INLINE gatherData #-}
type     DataSubGather d ds m = (DataGather ds m, DataLogging d m)
--
-- class     Monad m                                       => MonadLogging' ls        m where log' :: DataStore ls -> m ()
-- instance (Monad m, MonadLogging' ls (DataProvider l m)) => MonadLogging' (l ': ls) m where log' (l :-: ls) = provideData l $ log' ls ; {-# INLINE log' #-}
-- instance (RequiredDataGather m, MonadLogger m)          => MonadLogging' '[]       m where log' _ = gatherRequiredData >>= submitLog ; {-# INLINE log' #-}
--
-- class Monad m => MonadLogging ls m where
--     log :: DataStore ls -> m ()
--     default log :: (MonadTrans t, MonadLogging ls m) => DataStore ls -> t m ()
--     log = lift . log ; {-# INLINE log #-}
--
--
instance MonadLogging m => MonadLogging (StateT s m)
instance MonadLogging m => MonadLogging (IdentityT m)
--
--
--
-- type StdLogging      m = MonadLogging '[Priority, Msg]           m
-- type ReportedLogging m = MonadLogging '[Priority, Reporter, Msg] m


-- stdLog :: (StdLogging m, Enum p, ToText t) => p -> t -> m ()
-- stdLog p m = log $ priority p :-: msg m :-: Null
-- {-# INLINE stdLog #-}

-- reportedLog :: (ReportedLogging m, Enum p, ToText r, ToText t) => p -> r -> t -> m ()
-- reportedLog p r m = log $ priority p :-: reporter r :-: msg m :-: Null
-- {-# INLINE reportedLog #-}
--
--
runStdLogger :: forall m a req. (Monad m, req ~ [Priority, Nesting, Msg]) => BaseLogger req m a -> m a
runStdLogger = runLogger @req ; {-# INLINE runStdLogger #-}


-----------------------------

type NewLog m = (RequiredDataGather m, MonadLogger m)
newLog :: NewLog m => m ()
newLog = gatherRequiredData >>= submitLog


class Monad m => MonadLogging m where
    log2 :: m ()
    default log2 :: (MonadTrans t, MonadLogging m) => t m ()
    log2 = lift log2 ; {-# INLINE log2 #-}

instance (Monad m, NewLog (DataProvider d m)) => MonadLogging (DataProvider d m) where log2 = newLog
instance (Monad m, NewLog (EchoLogger m)) => MonadLogging (EchoLogger m) where log2 = newLog

type Logging m = (MonadLogging m, DataSetLogging '[Priority, Msg, Nesting] m)

tst2 :: Logging m => m ()
tst2 = do
    withData (msg "foo") $ log2



stdLog :: (Logging m, Enum p, ToText t) => p -> t -> m ()
stdLog p t = withData (priority p) $ withData (msg t) log2



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


------------------------------------
-- === Standard logging utils === --
------------------------------------

debug, info, notice, warning, err, critical, alert, panic
    :: (Logging m, ToText t) => t -> m ()
debug    = stdLog Debug    ; {-# INLINE debug    #-}
info     = stdLog Info     ; {-# INLINE info     #-}
notice   = stdLog Notice   ; {-# INLINE notice   #-}
warning  = stdLog Warning  ; {-# INLINE warning  #-}
err      = stdLog Error    ; {-# INLINE err      #-}
critical = stdLog Critical ; {-# INLINE critical #-}
alert    = stdLog Alert    ; {-# INLINE alert    #-}
panic    = stdLog Panic    ; {-# INLINE panic    #-}

withDebug, withInfo, withNotice, withWarning, withError, withCritical, withAlert, withPanic
    :: (Logging m, ToText t) => t -> m a -> m a
withDebug    s f = debug    s >> nested f ; {-# INLINE withDebug    #-}
withInfo     s f = info     s >> nested f ; {-# INLINE withInfo     #-}
withNotice   s f = notice   s >> nested f ; {-# INLINE withNotice   #-}
withWarning  s f = warning  s >> nested f ; {-# INLINE withWarning  #-}
withError    s f = err      s >> nested f ; {-# INLINE withError    #-}
withCritical s f = critical s >> nested f ; {-# INLINE withCritical #-}
withAlert    s f = alert    s >> nested f ; {-# INLINE withAlert    #-}
withPanic    s f = panic    s >> nested f ; {-# INLINE withPanic    #-}


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

reporter :: ToText t => t -> LogData Reporter
reporter = wrap' . convert ; {-# INLINE reporter #-}


-- === Priority === --

data Priority
type instance DataOf Priority = Int

priority :: Enum p => p -> LogData Priority
priority = wrap' . fromEnum ; {-# INLINE priority #-}


-- === Nesting === --

data Nesting
type instance DataOf Nesting = Int
type NestedLogging = DataLogging Nesting

nesting :: Int -> LogData Nesting
nesting = wrap' ; {-# INLINE nesting #-}

incNesting :: NestedLogging m => m ()
incNesting = modifyData_ @Nesting succ ; {-# INLINE incNesting #-}

decNesting :: NestedLogging m => m ()
decNesting = modifyData_ @Nesting pred ; {-# INLINE decNesting #-}

nested :: NestedLogging m => m a -> m a
nested = withModData @Nesting succ ; {-# INLINE nested #-}



------------------------
-- === EchoLogger === --
------------------------

data Echo
type instance LoggerDefinition Echo = IdentityT
type EchoLogger = Logger '[] Echo

runEchoLogger :: EchoLogger m a -> m a
runEchoLogger = runIdentityT . unwrap' ; {-# INLINE runEchoLogger #-}



-- === Instances === --

-- instance (MonadLogging' ls (EchoLogger m), Monad m) => MonadLogging ls (EchoLogger m)
--     where log = log' ; {-# INLINE log #-}

instance (MonadIO m, MonadLogger m, RequiredLookups m '[Msg, Nesting]) => MonadLogger (EchoLogger m) where
    submitLog ds = putStrLn (ident <> msg) *> lift (submitLog ds) where
        space = replicate 2 ' '
        ident = concat $ replicate nest space
        msg   = convert $ fromJust $ lookupData' @Msg     ds -- FIXME [WD]: unsafe
        nest  =           fromJust $ lookupData' @Nesting ds -- FIXME [WD]: unsafe
        fromJust (Just a) = a

instance PrimMonad m => PrimMonad (EchoLogger m) where
    type PrimState (EchoLogger m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}


------------------------
-- === DropLogger === --
------------------------

newtype DropLogger m a = DropLogger (IdentityT m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)
makeWrapped ''DropLogger

dropLogs :: DropLogger m a -> m a
dropLogs = runIdentityT . unwrap' ; {-# INLINE dropLogs #-}


-- === Instances === --
--
-- instance (MonadLogging' ls (DropLogger m), Monad m) => MonadLogging ls (DropLogger m)
--     where log _ = return () ; {-# INLINE log #-}

instance MonadIO m => MonadLogger (DropLogger m) where
    submitLog _ = return () ; {-# INLINE submitLog #-}

instance PrimMonad m => PrimMonad (DropLogger m) where
    type PrimState (DropLogger m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}

-- hacky logs dropping

-- imp :: a
-- imp = error "impossible happened."
--
-- instance Monad m => NestedLogging (DropLogger m) where
--     getNesting   = return imp
--     putNesting _ = return imp

-------------------
-- === Tests === --
-------------------



--
--
--
-- -- logMsg ::
--

-- type Logging m = (StdLogging m, NestedLogging m)



tst :: Logging (IdentityT m) => m ()
tst = runIdentityT $ do
    withDebug "foo" $ do
        debug "bar"
    debug "baz"
    return ()

lmain :: IO ()
lmain = do
    runLogging $ runEchoLogger tst
    -- dropLogs tst

    -- runNestedLogger $ runStdLogger $ runEchoLogger tst2
    -- dropLogs tst
    print "hello"


runLogging = runStdLogger . provideData' @Nesting . provideData (priority Debug) . provideData (msg "")
