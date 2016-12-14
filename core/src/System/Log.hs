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


-- === Priority === --

newtype Pri  = Pri TypeRep deriving (Show, Eq, Ord)
makeWrapped ''Pri
type    Pris = [Pri]

instance IsTypeRep Pri

pris :: forall (ps :: [*]). Typeables ps => Pris
pris = typeReps' @ps ; {-# INLINE pris #-}


-- === BaseLogger Data === --

type family DataOf  a

newtype LogData d = LogData (DataOf d)
makeWrapped ''LogData

instance Default (DataOf d) => Default (LogData d) where
    def = wrap' def ; {-# INLINE def #-}


----------------------------
-- === PriorityLogger === --
----------------------------

newtype PriorityLogger m a = PriorityLogger (StateT Pris m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)
makeWrapped ''PriorityLogger

class    Monad m                                       => PriorityLogging m                  where getPris :: m Pris
instance Monad m                                       => PriorityLogging (PriorityLogger m) where getPris = wrap' State.get ; {-# INLINE getPris #-}
instance {-# OVERLAPPABLE #-} PriorityLoggingTrans t m => PriorityLogging (t m)              where getPris = lift getPris    ; {-# INLINE getPris #-}
type     PriorityLoggingTrans t m = (Monad m, Monad (t m), MonadTrans t, PriorityLogging m)

runPriorityLogger :: Monad m => Pris -> PriorityLogger m a -> m a
runPriorityLogger pris l = State.evalStateT (unwrap' l) pris ; {-# INLINE runPriorityLogger #-}


-- === Instances === --

instance PrimMonad m => PrimMonad (PriorityLogger m) where
    type PrimState (PriorityLogger m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}


----------------------------
-- === NestedLogger === --
----------------------------

newtype NestedLogger m a = NestedLogger (StateT Int m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)
makeWrapped ''NestedLogger

class Monad m => NestedLogging m where
    getNesting :: m Int
    putNesting :: Int -> m ()

instance Monad m => NestedLogging (NestedLogger m) where
    getNesting = wrap' State.get   ; {-# INLINE getNesting #-}
    putNesting = wrap' . State.put ; {-# INLINE putNesting #-}

type NestedLoggingTrans t m = (Monad m, Monad (t m), MonadTrans t, NestedLogging m)
instance {-# OVERLAPPABLE #-} NestedLoggingTrans t m => NestedLogging (t m) where
    getNesting = lift getNesting   ; {-# INLINE getNesting #-}
    putNesting = lift . putNesting ; {-# INLINE putNesting #-}


runNestedLogger :: Monad m => NestedLogger m a -> m a
runNestedLogger = flip State.evalStateT def . unwrap' ; {-# INLINE runNestedLogger #-}

modifyNesting :: NestedLogging m => (Int -> Int) -> m ()
modifyNesting f = do
    n <- getNesting
    putNesting $ f n
{-# INLINE modifyNesting #-}

incNesting :: NestedLogging m => m ()
incNesting = modifyNesting succ ; {-# INLINE incNesting #-}

decNesting :: NestedLogging m => m ()
decNesting = modifyNesting pred ; {-# INLINE decNesting #-}

nested :: NestedLogging m => m a -> m a
nested f = do
    incNesting
    out <- f
    decNesting
    return out
{-# INLINE nested #-}

-- === Instances === --

instance PrimMonad m => PrimMonad (NestedLogger m) where
    type PrimState (NestedLogger m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}


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

class    Monad m                                   => MonadLogger m              where submitLog :: DataStore' m -> m ()
instance Monad m                                   => MonadLogger (BaseLogger req m) where submitLog _ = return ()      ; {-# INLINE submitLog #-}
instance {-# OVERLAPPABLE #-} MonadLoggerTrans t m => MonadLogger (t m)          where submitLog = lift . submitLog ; {-# INLINE submitLog #-}
type MonadLoggerTrans t m = (Monad m, Monad (t m), MonadTrans t, MonadLogger m, RequiredData m ~ RequiredData (t m))

instance MonadLogger m => MonadLogger (DataProvider d m) where submitLog = lift . submitLog ; {-# INLINE submitLog #-}
instance MonadLogger m => MonadLogger (IdentityT      m) where submitLog = lift . submitLog ; {-# INLINE submitLog #-}
instance MonadLogger m => MonadLogger (StateT       s m) where submitLog = lift . submitLog ; {-# INLINE submitLog #-}

-- === Running === --

runLogger :: Monad m => BaseLogger req m a -> m a
runLogger = runIdentityT . unwrap' ; {-# INLINE runLogger #-}


-- === Data Provider === --

newtype DataProvider d m a = DataProvider (StateT (LogData d) m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)
makeWrapped ''DataProvider

provideData :: forall d m a. Monad m => LogData d -> DataProvider d m a -> m a
provideData = flip (State.evalStateT . unwrap') ; {-# INLINE provideData #-}


class     Monad m                   => ProvidedData d m                  where getData :: m (LogData d)
instance  Monad m                   => ProvidedData d (DataProvider d m) where getData = wrap' State.get  ; {-# INLINE getData #-}
instance (Monad m, DefaultData m d) => ProvidedData d (BaseLogger req m)     where getData = lift defaultData ; {-# INLINE getData #-}
instance {-# OVERLAPPABLE #-}
         ProvidedDataTrans d t m    => ProvidedData d (t m)              where getData = lift getData     ; {-# INLINE getData #-}
type     ProvidedDataTrans d t m = (Monad m, MonadTrans t, Monad (t m), ProvidedData d m)



instance {-# OVERLAPPABLE #-}
         (Monad m, Default (LogData d)) => DefaultData m d where defaultData = return def
class     Monad m                       => DefaultData m d where defaultData :: m (LogData d)



type RequiredDataGather m = DataGather (RequiredData m) m
gatherRequiredData :: RequiredDataGather m => m (DataStore' m)
gatherRequiredData = gatherData ; {-# INLINE gatherRequiredData #-}


class    Monad m              => DataGather ds        m where gatherData :: m (DataStore ds)
instance Monad m              => DataGather '[]       m where gatherData = return Null                      ; {-# INLINE gatherData #-}
instance DataSubGather d ds m => DataGather (d ': ds) m where gatherData = (:-:) <$> getData <*> gatherData ; {-# INLINE gatherData #-}
type     DataSubGather d ds m = (DataGather ds m, ProvidedData d m)

class     Monad m                                       => MonadLogging' ls        m where log' :: DataStore ls -> m ()
instance (Monad m, MonadLogging' ls (DataProvider l m)) => MonadLogging' (l ': ls) m where log' (l :-: ls) = provideData l $ log' ls ; {-# INLINE log' #-}
instance (RequiredDataGather m, MonadLogger m)          => MonadLogging' '[]       m where log' _ = gatherRequiredData >>= submitLog ; {-# INLINE log' #-}

class Monad m => MonadLogging ls m where
    log :: DataStore ls -> m ()
    default log :: (MonadTrans t, MonadLogging ls m) => DataStore ls -> t m ()
    log = lift . log ; {-# INLINE log #-}



instance MonadLogging ls m => MonadLogging ls (StateT s m)
instance MonadLogging ls m => MonadLogging ls (IdentityT m)




type Logging m = (MonadLogging '[Priority, Msg] m, PriorityLogging m, NestedLogging m)


stdLog :: (Logging m, Typeable p, ToText t) => p -> t -> m ()
stdLog p m = do
    pri <- priority p
    log $ pri :-: msg m :-: Null
{-# INLINE stdLog #-}

runLogging :: forall req m a. Monad m => Pris -> PriorityLogger (BaseLogger req m) a -> m a
runLogging pris = runLogger . runPriorityLogger pris ; {-# INLINE runLogging #-}

runStdLogging :: forall req m a. Monad m => PriorityLogger (BaseLogger req m) a -> m a
runStdLogging = runLogging stdPris ; {-# INLINE runStdLogging #-}

runStdLogging' :: forall m a req. (Monad m, req ~ [Priority, Nesting, Msg]) => PriorityLogger (BaseLogger req m) a -> m a
runStdLogging' = runStdLogging @req ; {-# INLINE runStdLogging' #-}


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

stdPris :: Pris
stdPris = pris @'[Debug, Info, Notice, Warning, Error, Critical, Alert, Panic]


------------------------------------
-- === Standard logging utils === --
------------------------------------

debug, info, notice, warning, err, critical, alert, panic :: (Logging m, ToText t) => t -> m ()
debug    = stdLog Debug    ; {-# INLINE debug    #-}
info     = stdLog Info     ; {-# INLINE info     #-}
notice   = stdLog Notice   ; {-# INLINE notice   #-}
warning  = stdLog Warning  ; {-# INLINE warning  #-}
err      = stdLog Error    ; {-# INLINE err      #-}
critical = stdLog Critical ; {-# INLINE critical #-}
alert    = stdLog Alert    ; {-# INLINE alert    #-}
panic    = stdLog Panic    ; {-# INLINE panic    #-}

withDebug, withInfo, withNotice, withWarning, withError, withCritical, withAlert, withPanic :: (Logging m, ToText t) => t -> m a -> m a
withDebug    s f = debug    s >> nested f ; {-# INLINE withDebug    #-}
withInfo     s f = info     s >> nested f ; {-# INLINE withInfo     #-}
withNotice   s f = notice   s >> nested f ; {-# INLINE withNotice   #-}
withWarning  s f = warning  s >> nested f ; {-# INLINE withWarning  #-}
withError    s f = err      s >> nested f ; {-# INLINE withError    #-}
withCritical s f = critical s >> nested f ; {-# INLINE withCritical #-}
withAlert    s f = alert    s >> nested f ; {-# INLINE withAlert    #-}
withPanic    s f = panic    s >> nested f ; {-# INLINE withPanic    #-}


--------------------------
-- === BaseLogger datas === --
--------------------------

-- === Msg === --

data Msg
type instance DataOf Msg = Text

msg :: ToText t => t -> LogData Msg
msg = wrap' . convert ; {-# INLINE msg #-}


-- === Priority === --

data Priority
type instance DataOf Priority = Int

priority :: (Typeable p, PriorityLogging m) => p -> m (LogData Priority)
priority p = do
    pris <- getPris
    let Just idx = elemIndex (Pri $ typeOf p) pris -- FIXME[WD]: what to do when the priority is not found?
    return $ wrap' idx
{-# INLINE priority #-}


-- === Nesting === --

data Nesting
type instance DataOf Nesting = Int

nesting :: Int -> LogData Nesting
nesting = wrap' ; {-# INLINE nesting #-}

instance NestedLogging m => DefaultData m Nesting where
    defaultData = nesting <$> getNesting ; {-# INLINE defaultData #-}


------------------------
-- === EchoLogger === --
------------------------

newtype EchoLogger m a = EchoLogger (IdentityT m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)
makeWrapped ''EchoLogger

runEchoLogger :: EchoLogger m a -> m a
runEchoLogger = runIdentityT . unwrap' ; {-# INLINE runEchoLogger #-}


-- === Instances === --

instance (MonadLogging' ls (EchoLogger m), Monad m) => MonadLogging ls (EchoLogger m)
    where log = log' ; {-# INLINE log #-}

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

instance (MonadLogging' ls (DropLogger m), Monad m) => MonadLogging ls (DropLogger m)
    where log _ = return () ; {-# INLINE log #-}

instance MonadIO m => MonadLogger (DropLogger m) where
    submitLog _ = return () ; {-# INLINE submitLog #-}

instance PrimMonad m => PrimMonad (DropLogger m) where
    type PrimState (DropLogger m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}

-- hacky logs dropping

imp :: a
imp = error "impossible happened."

instance Monad m => PriorityLogging (DropLogger m) where
    getPris = return imp

instance Monad m => NestedLogging (DropLogger m) where
    getNesting   = return imp
    putNesting _ = return imp

-------------------
-- === Tests === --
-------------------


--
--
--
-- -- logMsg ::
--
tst :: Logging (IdentityT m) => m ()
tst = runIdentityT $ do
    withDebug "foo" $ do
        debug "bar"
    debug "baz"
    return ()
--
lmain :: IO ()
lmain = do
    runNestedLogger $ runStdLogging' $ runEchoLogger tst
    dropLogs tst
    -- dropLogs tst
    print "hello"
