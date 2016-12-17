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
import System.Log.Logger.Writer as X



--------------------------------------------------------
--------------------------------------------------------













-- #ifdef IgnoreDebug



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

-- === Definition === --

data PLAIN
type PlainLogger = IdentityLogger PLAIN


-- === Utils === --

clearStyles :: DataStore Msg m => m ()
clearStyles = modifyData_ @Msg (wrapped %~ Doc.plain) ; {-# INLINE clearStyles #-}

plain :: Logger PlainLogger m a -> m a
plain = runIdentityLogger ; {-# INLINE plain #-}


-- === Instances === --

instance DataStore Msg m => IsLogger PlainLogger m where
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
