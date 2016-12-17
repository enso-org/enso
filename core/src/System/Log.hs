{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE NoOverloadedStrings  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs                #-}

module System.Log (module System.Log, module X) where

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


import System.Log.Data   as X
import System.Log.Logger.Class as X
import System.Log.Level  as X
import System.Log.Logger.Echo as X
import System.Log.Logger.Drop as X
import System.Log.Logger.Priority as X



--------------------------------------------------------
--------------------------------------------------------











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


type Logging        m = (LocLogging m, ReportedLogging m, NestedLogging m, MonadTags StdLevels m)


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
defPriorityFormatter :: DataStores '[Msg, Priority] m => Formatter m
defPriorityFormatter = "[" <:> Priority <:> "] " <:> Msg <:> Doc.hardline ; {-# INLINE defPriorityFormatter #-}

defTagFormatter :: DataStores '[DynTags, Msg] m => Formatter m
defTagFormatter = ("[" <:> DynTags <:> "] ") <:> Msg <:> Doc.hardline ; {-# INLINE defTagFormatter #-}

nestedReportedFormatter :: DataStores '[DynTags, Nesting, Reporter, Msg] m => Formatter m
nestedReportedFormatter = tagColorFormatter (buildFormatter "• ") <:> Nesting <:> Reporter <:> ": " <:> Msg <:> Doc.hardline ; {-# INLINE nestedReportedFormatter #-}
-- nestedReportedFormatter = ("[" <:> Priority % Compact <:> "] ") <:> Nesting <:> Reporter <:> ": " <:> Msg <:> Doc.hardline ; {-# INLINE nestedReportedFormatter #-}

-- defaultTimeFormatter :: DataStores '[Time, Loc, Priority, Msg] m => Formatter m
-- defaultTimeFormatter = ("[" <:> Priority % Compact <:> "] ") <:> Time <:> ": " <:> Loc <:> ": " <:> Msg <:> Doc.hardline ; {-# INLINE defaultTimeFormatter #-}

-- colorLvlFormatter :: DataStore Priority m => Formatter m -> Formatter m
-- colorLvlFormatter f = Formatter ((lvlColor . toEnum <$> getData' @Priority) <*> runFormatter f)

tagColorFormatter :: DataStore DynTags m => Formatter m -> Formatter m
tagColorFormatter f = Formatter ((checkTagsColor stdColorPalette <$> getData' @DynTags) <*> runFormatter f)


type Color = Doc -> Doc

type ColorPalette = Map DynTag Color

stdColorPalette :: ColorPalette
stdColorPalette = Map.insert (dynTag Debug)    Doc.blue
                $ Map.insert (dynTag Info)     Doc.green
                $ Map.insert (dynTag Notice)   Doc.green
                $ Map.insert (dynTag Warning)  Doc.yellow
                $ Map.insert (dynTag Error)    Doc.red
                $ Map.insert (dynTag Critical) Doc.red
                $ Map.insert (dynTag Alert)    Doc.red
                $ Map.insert (dynTag Panic)    Doc.red
                $ mempty

checkTagsColor :: ColorPalette -> Set DynTag -> Color
checkTagsColor p ts = case catMaybes $ map (flip Map.lookup p) $ convert ts of
    []    -> id
    (c:_) -> c
{-# INLINE checkTagsColor #-}




instance Pretty (LogData Nesting) where
    pretty (LogData n) = fold $ replicate (2 * n) Doc.space ; {-# INLINE pretty #-}

instance Pretty (LogData Msg) where
    pretty = unwrap' ; {-# INLINE pretty #-}

instance Pretty (LogData Reporter) where
    pretty = pretty . unwrap' ; {-# INLINE pretty #-}

instance Pretty (LogData Priority) where
    pretty = text . show . unwrap' ; {-# INLINE pretty #-}

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
    -- runTaggedLogging $ runEchoLogger $ runFormatLogger defTagFormatter $ tst
    runTaggedLogging $ runEchoLogger $ runFormatLogger defTagFormatter $ tst
    runPriorityLogging $ runPriorityLogger @StdLevels $ runEchoLogger $ runFormatLogger defPriorityFormatter $ tst
    -- let logs = runIdentity $ runPriorityLogging $ execWriterLogger @Msg $ tst
    -- print "---"
    -- print logs
    -- dropLogs tst

    -- runNestedLogger $ runStdLogger $ runEchoLogger tst
    -- dropLogs tst
    print "hello"
