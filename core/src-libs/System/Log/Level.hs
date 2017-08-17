{-# LANGUAGE NoMonomorphismRestriction #-} -- FIXME[WD]: remove
{-# LANGUAGE CPP #-}

module System.Log.Level where

import Prologue_old  hiding (nested)
import qualified GHC.Stack as Stack
import           GHC.Stack (HasCallStack, callStack, getCallStack)

import System.Log.Data
import System.Log.Logger.Class
import System.Log.Logger.Priority           (PriorityLogger, runPriorityLogger)
import Text.PrettyPrint.ANSI.Leijen.Convert (IsDoc)
import Text.PrettyPrint.ANSI.Leijen         (Doc)


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

type StdLevels = '[Debug, Info, Notice, Warning, Error, Critical, Alert, Panic]

type family Priorities (lvls :: [*]) (lvl :: *) :: [*] where
    Priorities '[]       l = '[]
    Priorities (l ': ls) l = l ': ls
    Priorities (l ': ls) k = Priorities ls k

type StdPriorities p = Priorities StdLevels p

stdLevels :: [TypeRep]
stdLevels = typeReps' @StdLevels

stdPriorities :: Typeable p => p -> [TypeRep]
stdPriorities p = dropWhile (/= (typeOf p)) stdLevels



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


-- === Generic logging === --

type LocLog m = (DataStore Loc m) -- , HasCallStack -- WARNING: GHC confuses HasCallStack here

type     MsgLog'     m = (MonadLogging  m, DataStore Msg      m)
type     TagLog'   t m = (MsgLog'       m, MonadTagged      t m)
type     TagLogBy' t m = (TagLog'     t m, DataStore Reporter m)
type WithTagLog'   t m = (TagLog'     t m, DataStore Nesting  m)
type WithTagLogBy' t m = (WithTagLog' t m, DataStore Reporter m)

type     TagLog    t m = (LocLog m,     TagLog'   t m)
type     TagLogBy  t m = (LocLog m,     TagLogBy' t m)
type WithTagLog    t m = (LocLog m, WithTagLog'   t m)
type WithTagLogBy  t m = (LocLog m, WithTagLogBy' t m)


addCallStackInfo :: (HasCallStack, DataStore Loc m) => m a -> m a
addCallStackInfo = withData (loc . snd . head . tail $ getCallStack callStack)


msgLog' :: MsgLog' m => Doc -> m ()
msgLog' m = withData (msg m) runLoggers

tagLog' :: forall t m. TagLog' t m => Doc -> m ()
tagLog' = tagged @t . msgLog'

tagLogBy' :: forall t m. TagLogBy' t m => Text -> Doc -> m ()
tagLogBy' r = reported r . tagLog' @t

withTagLog' :: forall t m a. WithTagLog' t m => Doc -> m a -> m a
withTagLog' m f = tagLog' @t m >> nested f

withTagLogBy' :: forall t m a. WithTagLogBy' t m => Text -> Doc -> m a -> m a
withTagLogBy' r m f = tagLogBy' @t r m >> nested (reported r f)


-- === Loggign utils === --

debug    :: (HasCallStack, TagLog Debug    m) => Doc -> m ()
info     :: (HasCallStack, TagLog Info     m) => Doc -> m ()
notice   :: (HasCallStack, TagLog Notice   m) => Doc -> m ()
warning  :: (HasCallStack, TagLog Warning  m) => Doc -> m ()
err      :: (HasCallStack, TagLog Error    m) => Doc -> m ()
critical :: (HasCallStack, TagLog Critical m) => Doc -> m ()
alert    :: (HasCallStack, TagLog Alert    m) => Doc -> m ()
panic    :: (HasCallStack, TagLog Panic    m) => Doc -> m ()

debug    = addCallStackInfo . tagLog' @Debug
info     = addCallStackInfo . tagLog' @Info
notice   = addCallStackInfo . tagLog' @Notice
warning  = addCallStackInfo . tagLog' @Warning
err      = addCallStackInfo . tagLog' @Error
critical = addCallStackInfo . tagLog' @Critical
alert    = addCallStackInfo . tagLog' @Alert
panic    = addCallStackInfo . tagLog' @Panic


debugBy    :: (HasCallStack, TagLogBy Debug    m) => Text -> Doc -> m ()
infoBy     :: (HasCallStack, TagLogBy Info     m) => Text -> Doc -> m ()
noticeBy   :: (HasCallStack, TagLogBy Notice   m) => Text -> Doc -> m ()
warningBy  :: (HasCallStack, TagLogBy Warning  m) => Text -> Doc -> m ()
errBy      :: (HasCallStack, TagLogBy Error    m) => Text -> Doc -> m ()
criticalBy :: (HasCallStack, TagLogBy Critical m) => Text -> Doc -> m ()
alertBy    :: (HasCallStack, TagLogBy Alert    m) => Text -> Doc -> m ()
panicBy    :: (HasCallStack, TagLogBy Panic    m) => Text -> Doc -> m ()

debugBy    = addCallStackInfo .: tagLogBy' @Debug
infoBy     = addCallStackInfo .: tagLogBy' @Info
noticeBy   = addCallStackInfo .: tagLogBy' @Notice
warningBy  = addCallStackInfo .: tagLogBy' @Warning
errBy      = addCallStackInfo .: tagLogBy' @Error
criticalBy = addCallStackInfo .: tagLogBy' @Critical
alertBy    = addCallStackInfo .: tagLogBy' @Alert
panicBy    = addCallStackInfo .: tagLogBy' @Panic


withDebug    :: (HasCallStack, WithTagLog Debug    m) => Doc -> m a -> m a
withInfo     :: (HasCallStack, WithTagLog Info     m) => Doc -> m a -> m a
withNotice   :: (HasCallStack, WithTagLog Notice   m) => Doc -> m a -> m a
withWarning  :: (HasCallStack, WithTagLog Warning  m) => Doc -> m a -> m a
withErr      :: (HasCallStack, WithTagLog Error    m) => Doc -> m a -> m a
withCritical :: (HasCallStack, WithTagLog Critical m) => Doc -> m a -> m a
withAlert    :: (HasCallStack, WithTagLog Alert    m) => Doc -> m a -> m a
withPanic    :: (HasCallStack, WithTagLog Panic    m) => Doc -> m a -> m a
#ifdef DEBUG
withDebug    = addCallStackInfo .: withTagLog' @Debug
withInfo     = addCallStackInfo .: withTagLog' @Info
withNotice   = addCallStackInfo .: withTagLog' @Notice
withWarning  = addCallStackInfo .: withTagLog' @Warning
withErr      = addCallStackInfo .: withTagLog' @Error
withCritical = addCallStackInfo .: withTagLog' @Critical
withAlert    = addCallStackInfo .: withTagLog' @Alert
withPanic    = addCallStackInfo .: withTagLog' @Panic
#else
withDebug    = flip const
withInfo     = flip const
withNotice   = flip const
withWarning  = flip const
withErr      = flip const
withCritical = flip const
withAlert    = flip const
withPanic    = flip const
#endif

withDebugBy    :: (HasCallStack, WithTagLogBy Debug    m) => Text -> Doc -> m a -> m a
withInfoBy     :: (HasCallStack, WithTagLogBy Info     m) => Text -> Doc -> m a -> m a
withNoticeBy   :: (HasCallStack, WithTagLogBy Notice   m) => Text -> Doc -> m a -> m a
withWarningBy  :: (HasCallStack, WithTagLogBy Warning  m) => Text -> Doc -> m a -> m a
withErrBy      :: (HasCallStack, WithTagLogBy Error    m) => Text -> Doc -> m a -> m a
withCriticalBy :: (HasCallStack, WithTagLogBy Critical m) => Text -> Doc -> m a -> m a
withAlertBy    :: (HasCallStack, WithTagLogBy Alert    m) => Text -> Doc -> m a -> m a
withPanicBy    :: (HasCallStack, WithTagLogBy Panic    m) => Text -> Doc -> m a -> m a
#ifdef DEBUG
withDebugBy    = addCallStackInfo .:. withTagLogBy' @Debug
withInfoBy     = addCallStackInfo .:. withTagLogBy' @Info
withNoticeBy   = addCallStackInfo .:. withTagLogBy' @Notice
withWarningBy  = addCallStackInfo .:. withTagLogBy' @Warning
withErrBy      = addCallStackInfo .:. withTagLogBy' @Error
withCriticalBy = addCallStackInfo .:. withTagLogBy' @Critical
withAlertBy    = addCallStackInfo .:. withTagLogBy' @Alert
withPanicBy    = addCallStackInfo .:. withTagLogBy' @Panic
#else
withDebugBy    = const $ flip const
withInfoBy     = const $ flip const
withNoticeBy   = const $ flip const
withWarningBy  = const $ flip const
withErrBy      = const $ flip const
withCriticalBy = const $ flip const
withAlertBy    = const $ flip const
withPanicBy    = const $ flip const
#endif


-- === Running utils === --

type Logging m = (LocLogging m, ReportedLogging m, NestedLogging m, MonadTags StdLevels m)

runPriorityLogging :: forall prs m a. Monad m
                   => Logger (PriorityLogger prs) (DataProviderStack '[Loc, Msg, Reporter, Nesting, Priority] m) a -> m a
runPriorityLogging = provideData 0 -- FIXME: Is there any better way to provide non-yet-set priority?
           . provideDefData @Nesting
           . provideDefData @Reporter
           . provideDefData @Msg
           . provideData    (loc unknownSrcLoc)
           . runPriorityLogger @prs
{-# INLINE runPriorityLogging #-}

runTaggedLogging :: Monad m
                 => DataProviderStack '[Loc, Msg, Reporter, Nesting, DynTags] m a -> m a
runTaggedLogging = provideDefData @DynTags
                 . provideDefData @Nesting
                 . provideDefData @Reporter
                 . provideDefData @Msg
                 . provideData  (loc unknownSrcLoc)
{-# INLINE runTaggedLogging #-}

unknownSrcLoc :: Stack.SrcLoc
unknownSrcLoc = Stack.SrcLoc unknown unknown unknown 0 0 0 0 where unknown = "Unknown"
