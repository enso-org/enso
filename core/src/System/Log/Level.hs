{-# LANGUAGE NoMonomorphismRestriction #-} -- FIXME[WD]: remove

module System.Log.Level where

import Prologue  hiding (nested)
import qualified GHC.Stack as Stack
import           GHC.Stack (HasCallStack, callStack, getCallStack)

import System.Log.Data
import System.Log.Logger.Class
import System.Log.Logger.Priority           (PriorityLogger, runPriorityLogger)
import Text.PrettyPrint.ANSI.Leijen.Convert (IsDoc)



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
stdLevels = typeReps' @StdLevels ; {-# INLINE stdLevels #-}

stdPriorities :: Typeable p => p -> [TypeRep]
stdPriorities p = dropWhile (/= (typeOf p)) stdLevels ; {-# INLINE stdPriorities #-}



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

type     MsgLog'              msg m = (MonadLogging      m, DataStore Msg      m, IsDoc msg)
type     TagLog'   t          msg m = (MsgLog'       msg m, MonadTagged t      m)
type     TagLogBy' t reporter msg m = (TagLog'     t msg m, DataStore Reporter m, ToText reporter)
type WithTagLog'   t          msg m = (TagLog'     t msg m, DataStore Nesting  m)
type WithTagLogBy' t reporter msg m = (WithTagLog' t msg m, DataStore Reporter m, ToText reporter)

type     TagLog    t          msg m = (LocLog m,     TagLog'   t          msg m)
type     TagLogBy  t reporter msg m = (LocLog m,     TagLogBy' t reporter msg m)
type WithTagLog    t          msg m = (LocLog m, WithTagLog'   t          msg m)
type WithTagLogBy  t reporter msg m = (LocLog m, WithTagLogBy' t reporter msg m)


addCallStackInfo :: (HasCallStack, DataStore Loc m) => m a -> m a
addCallStackInfo = withData (loc . snd . head . tail $ getCallStack callStack)


msgLog' :: forall msg m. MsgLog' msg m
        => msg -> m ()
msgLog' m = withData (msg m) runLoggers ; {-# INLINE msgLog' #-}

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
withTagLogBy' r m f = tagLogBy' @t r m >> nested (reported r f) ; {-# INLINE withTagLogBy' #-}


-- === Loggign utils === --

debug    :: (HasCallStack, TagLog Debug    msg m) => msg -> m ()
info     :: (HasCallStack, TagLog Info     msg m) => msg -> m ()
notice   :: (HasCallStack, TagLog Notice   msg m) => msg -> m ()
warning  :: (HasCallStack, TagLog Warning  msg m) => msg -> m ()
err      :: (HasCallStack, TagLog Error    msg m) => msg -> m ()
critical :: (HasCallStack, TagLog Critical msg m) => msg -> m ()
alert    :: (HasCallStack, TagLog Alert    msg m) => msg -> m ()
panic    :: (HasCallStack, TagLog Panic    msg m) => msg -> m ()

debug    = addCallStackInfo . tagLog' @Debug    ; {-# INLINE debug    #-}
info     = addCallStackInfo . tagLog' @Info     ; {-# INLINE info     #-}
notice   = addCallStackInfo . tagLog' @Notice   ; {-# INLINE notice   #-}
warning  = addCallStackInfo . tagLog' @Warning  ; {-# INLINE warning  #-}
err      = addCallStackInfo . tagLog' @Error    ; {-# INLINE err      #-}
critical = addCallStackInfo . tagLog' @Critical ; {-# INLINE critical #-}
alert    = addCallStackInfo . tagLog' @Alert    ; {-# INLINE alert    #-}
panic    = addCallStackInfo . tagLog' @Panic    ; {-# INLINE panic    #-}


debugBy    :: (HasCallStack, TagLogBy Debug    reporter msg m) => reporter -> msg -> m ()
infoBy     :: (HasCallStack, TagLogBy Info     reporter msg m) => reporter -> msg -> m ()
noticeBy   :: (HasCallStack, TagLogBy Notice   reporter msg m) => reporter -> msg -> m ()
warningBy  :: (HasCallStack, TagLogBy Warning  reporter msg m) => reporter -> msg -> m ()
errBy      :: (HasCallStack, TagLogBy Error    reporter msg m) => reporter -> msg -> m ()
criticalBy :: (HasCallStack, TagLogBy Critical reporter msg m) => reporter -> msg -> m ()
alertBy    :: (HasCallStack, TagLogBy Alert    reporter msg m) => reporter -> msg -> m ()
panicBy    :: (HasCallStack, TagLogBy Panic    reporter msg m) => reporter -> msg -> m ()

debugBy    = addCallStackInfo .: tagLogBy' @Debug    ; {-# INLINE debugBy    #-}
infoBy     = addCallStackInfo .: tagLogBy' @Info     ; {-# INLINE infoBy     #-}
noticeBy   = addCallStackInfo .: tagLogBy' @Notice   ; {-# INLINE noticeBy   #-}
warningBy  = addCallStackInfo .: tagLogBy' @Warning  ; {-# INLINE warningBy  #-}
errBy      = addCallStackInfo .: tagLogBy' @Error    ; {-# INLINE errBy      #-}
criticalBy = addCallStackInfo .: tagLogBy' @Critical ; {-# INLINE criticalBy #-}
alertBy    = addCallStackInfo .: tagLogBy' @Alert    ; {-# INLINE alertBy    #-}
panicBy    = addCallStackInfo .: tagLogBy' @Panic    ; {-# INLINE panicBy    #-}


withDebug    :: (HasCallStack, WithTagLog Debug    msg m) => msg -> m a -> m a
withInfo     :: (HasCallStack, WithTagLog Info     msg m) => msg -> m a -> m a
withNotice   :: (HasCallStack, WithTagLog Notice   msg m) => msg -> m a -> m a
withWarning  :: (HasCallStack, WithTagLog Warning  msg m) => msg -> m a -> m a
withErr      :: (HasCallStack, WithTagLog Error    msg m) => msg -> m a -> m a
withCritical :: (HasCallStack, WithTagLog Critical msg m) => msg -> m a -> m a
withAlert    :: (HasCallStack, WithTagLog Alert    msg m) => msg -> m a -> m a
withPanic    :: (HasCallStack, WithTagLog Panic    msg m) => msg -> m a -> m a

withDebug    = addCallStackInfo .: withTagLog' @Debug    ; {-# INLINE withDebug    #-}
withInfo     = addCallStackInfo .: withTagLog' @Info     ; {-# INLINE withInfo     #-}
withNotice   = addCallStackInfo .: withTagLog' @Notice   ; {-# INLINE withNotice   #-}
withWarning  = addCallStackInfo .: withTagLog' @Warning  ; {-# INLINE withWarning  #-}
withErr      = addCallStackInfo .: withTagLog' @Error    ; {-# INLINE withErr      #-}
withCritical = addCallStackInfo .: withTagLog' @Critical ; {-# INLINE withCritical #-}
withAlert    = addCallStackInfo .: withTagLog' @Alert    ; {-# INLINE withAlert    #-}
withPanic    = addCallStackInfo .: withTagLog' @Panic    ; {-# INLINE withPanic    #-}


withDebugBy    :: (HasCallStack, WithTagLogBy Debug    reporter msg m) => reporter -> msg -> m a -> m a
withInfoBy     :: (HasCallStack, WithTagLogBy Info     reporter msg m) => reporter -> msg -> m a -> m a
withNoticeBy   :: (HasCallStack, WithTagLogBy Notice   reporter msg m) => reporter -> msg -> m a -> m a
withWarningBy  :: (HasCallStack, WithTagLogBy Warning  reporter msg m) => reporter -> msg -> m a -> m a
withErrBy      :: (HasCallStack, WithTagLogBy Error    reporter msg m) => reporter -> msg -> m a -> m a
withCriticalBy :: (HasCallStack, WithTagLogBy Critical reporter msg m) => reporter -> msg -> m a -> m a
withAlertBy    :: (HasCallStack, WithTagLogBy Alert    reporter msg m) => reporter -> msg -> m a -> m a
withPanicBy    :: (HasCallStack, WithTagLogBy Panic    reporter msg m) => reporter -> msg -> m a -> m a

withDebugBy    = addCallStackInfo .:. withTagLogBy' @Debug    ; {-# INLINE withDebugBy    #-}
withInfoBy     = addCallStackInfo .:. withTagLogBy' @Info     ; {-# INLINE withInfoBy     #-}
withNoticeBy   = addCallStackInfo .:. withTagLogBy' @Notice   ; {-# INLINE withNoticeBy   #-}
withWarningBy  = addCallStackInfo .:. withTagLogBy' @Warning  ; {-# INLINE withWarningBy  #-}
withErrBy      = addCallStackInfo .:. withTagLogBy' @Error    ; {-# INLINE withErrBy      #-}
withCriticalBy = addCallStackInfo .:. withTagLogBy' @Critical ; {-# INLINE withCriticalBy #-}
withAlertBy    = addCallStackInfo .:. withTagLogBy' @Alert    ; {-# INLINE withAlertBy    #-}
withPanicBy    = addCallStackInfo .:. withTagLogBy' @Panic    ; {-# INLINE withPanicBy    #-}


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
