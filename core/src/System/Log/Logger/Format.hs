{-# LANGUAGE UndecidableInstances #-}

module System.Log.Logger.Format where

import Prologue hiding (Simple)

import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Set                     (Set)
import           Data.Time.Format             (formatTime)
import           Data.Time.Locale.Compat      (defaultTimeLocale)
import qualified Data.Typeable                as Typeable
import           Text.PrettyPrint.ANSI.Leijen (Doc, pretty, text)
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import qualified GHC.Stack                    as Stack

import System.Log.Logger.Class
import System.Log.Level
import System.Log.Data



-------------------
-- === Style === --
-------------------

-- === Definition === --

newtype Styled s a = Styled a deriving (Show, Functor, Foldable, Traversable)
makeWrapped ''Styled


-- === Utils === --

infixl 6 %
(%) :: forall a s. a -> s -> Styled s a
a % _ = wrap' a ; {-# INLINE (%) #-}


-- === Built-in style types === --

data Simple
data Compact



--------------------
-- === Render === --
--------------------

-- === Generic rendering === --

class Monad m => Render style a m where
    render :: a -> m Doc

instance {-# OVERLAPPABLE #-} (DataStore d m, RenderDataM s d m)
                 => Render s      d      m where render _ = renderDataM @s @d . unwrap' =<< getData @d ; {-# INLINE render #-}
instance Monad m => Render Simple String m where render   = return . pretty                            ; {-# INLINE render #-}
instance Monad m => Render Simple Doc    m where render   = return                                     ; {-# INLINE render #-}


-- === Monadic data rendering === --

class Monad m => RenderDataM style d m where
    renderDataM :: DataOf d -> m Doc

instance {-# OVERLAPPABLE #-} (Monad m, RenderData style d)
      => RenderDataM style d m where
    renderDataM = return . renderData @style @d ; {-# INLINE renderDataM #-}


-- === Pure data rendering === --

class RenderData style d where
    renderData :: DataOf d -> Doc


-- === Built-in data simple rendering === --

instance RenderData Simple Msg      where renderData     = id                                                                                    ; {-# INLINE renderData #-}
instance RenderData Simple Reporter where renderData     = text . convert {- FIXME: Text -> String -> Doc is not the optimal path. -}            ; {-# INLINE renderData #-}
instance RenderData Simple Priority where renderData     = pretty                                                                                ; {-# INLINE renderData #-}
instance RenderData Simple Time     where renderData     = text . formatTime defaultTimeLocale "%c"                                              ; {-# INLINE renderData #-}
instance RenderData Simple Nesting  where renderData n   = fold $ replicate (2 * n) Doc.space                                                    ; {-# INLINE renderData #-}
instance RenderData Simple Loc      where renderData loc = text $ Stack.srcLocFile loc <> ":" <> show (Stack.srcLocStartLine loc)                ; {-# INLINE renderData #-}
instance RenderData Simple DynTags  where renderData     = text . intercalate ", " . fmap (Typeable.tyConName . Typeable.typeRepTyCon) . convert ; {-# INLINE renderData #-}



-----------------------
-- === Formatter === --
-----------------------

-- === Definition === --

newtype Formatter m a = Formatter { runFormatter :: Doc -> m a } deriving (Functor)
makeWrapped ''Formatter


-- === Rendering === --

renderStyled :: forall s a m. Render s a m => Styled s a -> Formatter m Doc
renderStyled a = wrap' $ \doc -> (doc <>) <$> render @s (unwrap' a) ; {-# INLINE renderStyled #-}


-- === IsFormatter === --

class IsFormatter a m where
    formatter :: a -> Formatter m Doc

type IsSimpleFormatter a m = IsFormatter (Styled Simple a) m

instance {-# OVERLAPPABLE #-}
         IsSimpleFormatter a m => IsFormatter a                 m where formatter = formatter . Styled @Simple ; {-# INLINE formatter #-}
instance Render s d m          => IsFormatter (Styled s d)      m where formatter = renderStyled               ; {-# INLINE formatter #-}
instance (m ~ n)               => IsFormatter (Formatter n Doc) m where formatter = id                         ; {-# INLINE formatter #-}


-- === Combinators === --

infixr 5 <:>
(<:>) :: (Monad m, IsFormatter a m, IsFormatter b m) => a -> b -> Formatter m Doc
a <:> b = liftA2 mappend (formatter a) (formatter b) ; {-# INLINE (<:>) #-}

infixr 5 <+>
(<+>) :: (Monad m, IsFormatter a m, IsFormatter b m) => a -> b -> Formatter m Doc
a <+> b = a <:> " " <:> b

between :: (Monad m, IsFormatter l m, IsFormatter r m, IsFormatter a m) => l -> r -> a -> Formatter m Doc
between l r a = l <:> a <:> r ; {-# INLINE between #-}

bracked :: (Monad m, IsFormatter a m) => a -> Formatter m Doc
bracked = between "[" "]" ; {-# INLINE bracked #-}

parensed :: (Monad m, IsFormatter a m) => a -> Formatter m Doc
parensed = between "(" ")" ; {-# INLINE parensed #-}

space :: Monad m => Formatter m Doc
space = formatter " " ; {-# INLINE space #-}

eol :: Monad m => Formatter m Doc
eol = formatter Doc.hardline ; {-# INLINE eol #-}

line :: (Monad m, IsFormatter a m) => a -> Formatter m Doc
line = (<:> eol) ; {-# INLINE line #-}


-- === Instances === --

instance Monad m => Applicative (Formatter m) where
    pure    = wrap' . const . return                             ; {-# INLINE pure  #-}
    f <*> a = wrap' $ \doc -> unwrap' f doc <*> unwrap' a mempty ; {-# INLINE (<*>) #-}



------------------------------
-- === Color Formatters === --
------------------------------

-- TODO: make it easier for the end-user to overwrite colors and define custom palettes.

dynTagsColorFormatter :: (DataStore DynTags m, IsFormatter f m) => f -> Formatter m Doc
dynTagsColorFormatter f = Formatter $ \doc -> ((checkTagsColor stdColorPalette <$> getData' @DynTags) <*> runFormatter (formatter f) doc)

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



--------------------------
-- === FormatLogger === --
--------------------------

-- === Definition === --

data FORMAT
type FormatLogger = StateLogger FORMAT
type instance StateOf FORMAT m = Formatter m Doc


-- === Utils === --

type MonadFormatLogger = MonadLoggerState FORMAT

getFormatter :: MonadFormatLogger m => m (Formatter m Doc)
getFormatter = getLoggerState @FORMAT ; {-# INLINE getFormatter #-}

runFormatLogger :: Monad m => Formatter (Logger FormatLogger m) Doc -> Logger FormatLogger m a -> m a
runFormatLogger = flip evalStateLogger ; {-# INLINE runFormatLogger #-}


-- === Instances === --

instance DataStore Msg m => IsLogger FormatLogger m where
    runLogger = putData' @Msg =<< flip unwrap' mempty =<< getFormatter ; {-# INLINE runLogger #-}



--------------------------------
-- === Example formatters === --
--------------------------------

nestedColorFormatter :: DataStores '[DynTags, Nesting, Reporter, Msg] m => Formatter m Doc
-- nestedColorFormatter = line $ Nesting <:> dynTagsColorFormatter Msg <+> parensed Reporter ; {-# INLINE nestedColorFormatter #-}
nestedColorFormatter = line $ Nesting <:> Reporter <:> ":" <+> dynTagsColorFormatter Msg ; {-# INLINE nestedColorFormatter #-}

bulletNestingFormatter :: DataStores '[DynTags, Nesting, Reporter, Msg] m => Formatter m Doc
bulletNestingFormatter = line $ dynTagsColorFormatter (formatter "•") <+> Nesting <:> Reporter <:> ":" <+> Msg ; {-# INLINE bulletNestingFormatter #-}

examplePriorityFormatter :: DataStores '[Priority, Loc, Msg] m => Formatter m Doc
examplePriorityFormatter = line $ bracked Priority <+> Loc <:> ":" <+> Msg ; {-# INLINE examplePriorityFormatter #-}
