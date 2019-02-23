module Data.Text.Terminal where


import qualified Prologue as P
import Prologue hiding (Bounded, div, concat, putStr, length, putStrLn, take, drop)

import qualified Control.Monad.State.Layered as State
import qualified Data.Text                   as Text
import qualified Data.Text.IO                as Text
import qualified Data.Text.Lazy.Builder      as Text
import qualified System.Console.ANSI         as ANSI
import qualified System.IO                   as System
import           System.IO                 (Handle, stdout)

-- import Data.Container.Class hiding (intercalate)
-- import Data.Container.Sequence


-------------------
-- === Style === --
-------------------

-- === Definition === --

data Style = Style { _weightStyle    :: !WeightType
                   , _italicStyle    :: !Bool
                   , _underlineStyle :: !(Maybe UnderlineType)
                   , _blinkingStyle  :: !(Maybe BlinkSpeed)
                   , _swappedStyle   :: !Bool
                   , _fgColorStyle   :: !(Maybe Color)
                   , _bgColorStyle   :: !(Maybe Color)
                   } deriving (Show, Eq)

data StyleChange = WeightChange    !WeightType
                 | ItalicChange    !Bool
                 | UnderlineChange !(Maybe UnderlineType)
                 | BlinkingChange  !(Maybe BlinkSpeed)
                 | SwappedChange   !Bool
                 | FgColorChange   !(Maybe Color)
                 | BgColorChange   !(Maybe Color)
                 deriving (Show, Eq)

-- | We can set color using 3 ways:
--     - ANSIColor provides 8 different colors (each available in 2 intensivites - vivid and dull) and it is widely supported by almost all Unix terminals and older Windows releases
--     - Color256  provides 256 predefined colors and it is widely supported by almost all Unix terminals and Windows release 10 and newer (color chart: https://upload.wikimedia.org/wikipedia/commons/1/15/Xterm_256color_chart.svg)
--     - ColorRGB  provides 24bit true colour (16 milion colors) and it is supported by moder Unix terminals and Windows release 10 and newer
data Color = ANSIColor !ANSIColorIntensity !ANSIColorTone
           | Color256  !Word8
           | ColorRGB  !Word8 !Word8 !Word8
           deriving (Show, Eq)

data ANSIColorIntensity = Light
                        | Dark
                        deriving (Show, Eq)

data ANSIColorTone      = Black
                        | Red
                        | Green
                        | Yellow
                        | Blue
                        | Magenta
                        | Cyan
                        | White
                        deriving (Show, Eq, Enum)

data BlinkSpeed         = SlowBlink
                        | FastBlink
                        deriving (Show, Eq)

data WeightType         = NormalWeight
                        | BoldWeight
                        | ThinWeight -- | Not widely supported: sometimes treated as concealing text
                        deriving (Show, Eq)

data UnderlineType      = SingleUnderline
                        | DoubleUnderline
                        deriving (Show, Eq)

makeLenses ''Style


-- === Utils === --

resetSGR :: MonadIO m => Handle -> m ()
resetSGR h = liftIO $ ANSI.hSetSGR h [ANSI.Reset]

getStyleAsChanges :: State.Getter Style m => m [StyleChange]
getStyleAsChanges = styleToChanges <$> State.get @Style

styleToChanges :: Style -> [StyleChange]
styleToChanges = diffStyles def

getReversedStyleChange :: State.Getter Style m => StyleChange -> m StyleChange
getReversedStyleChange change = do
    s <- State.get @Style
    pure $ case change of
        WeightChange    _ -> WeightChange    $ s ^. weightStyle
        ItalicChange    _ -> ItalicChange    $ s ^. italicStyle
        UnderlineChange _ -> UnderlineChange $ s ^. underlineStyle
        BlinkingChange  _ -> BlinkingChange  $ s ^. blinkingStyle
        SwappedChange   _ -> SwappedChange   $ s ^. swappedStyle
        FgColorChange   _ -> FgColorChange   $ s ^. fgColorStyle
        BgColorChange   _ -> BgColorChange   $ s ^. bgColorStyle


putStyleChange :: State.Monad Style m => StyleChange -> m ()
putStyleChange ch = State.modify_ @Style $ case ch of
    WeightChange    a -> weightStyle    .~ a
    ItalicChange    a -> italicStyle    .~ a
    UnderlineChange a -> underlineStyle .~ a
    BlinkingChange  a -> blinkingStyle  .~ a
    SwappedChange   a -> swappedStyle   .~ a
    FgColorChange   a -> fgColorStyle   .~ a
    BgColorChange   a -> bgColorStyle   .~ a

withStyleChanges :: (State.Monad Style m, MonadIO m) => Handle -> [StyleChange] -> m a -> m a
withStyleChanges h changes m = do
    reverseChanges <- mapM getReversedStyleChange changes
    hSetStyleChanges h changes
    out <- m
    hSetStyleChanges h reverseChanges
    pure out


diffStyles :: Style -> Style -> [StyleChange]
diffStyles s s' = weightDiff . italicDiff . underlineDiff . blinkingDiff . swappedDiff . fgColorDiff . bgColorDiff $ mempty where
    weightDiff    = if s ^. weightStyle    == s' ^. weightStyle    then id else (WeightChange    (s' ^. weightStyle)    :)
    italicDiff    = if s ^. italicStyle    == s' ^. italicStyle    then id else (ItalicChange    (s' ^. italicStyle)    :)
    underlineDiff = if s ^. underlineStyle == s' ^. underlineStyle then id else (UnderlineChange (s' ^. underlineStyle) :)
    blinkingDiff  = if s ^. blinkingStyle  == s' ^. blinkingStyle  then id else (BlinkingChange  (s' ^. blinkingStyle)  :)
    swappedDiff   = if s ^. swappedStyle   == s' ^. swappedStyle   then id else (SwappedChange   (s' ^. swappedStyle)   :)
    fgColorDiff   = if s ^. fgColorStyle   == s' ^. fgColorStyle   then id else (FgColorChange   (s' ^. fgColorStyle)   :)
    bgColorDiff   = if s ^. bgColorStyle   == s' ^. bgColorStyle   then id else (BgColorChange   (s' ^. bgColorStyle)   :)


hSetStyleChanges :: (State.Monad Style m, MonadIO m) => Handle -> [StyleChange] -> m ()
hSetStyleChanges h changes = do
    let colorReser = (FgColorChange Nothing `elem` changes) || (BgColorChange Nothing `elem` changes)
    mapM putStyleChange changes
    _        <- State.get @Style
    changes' <- if colorReser then resetSGR h >> getStyleAsChanges
                              else pure changes
    liftIO $ mapM_ apply changes'
    where setSGR            = ANSI.hSetSGR h . pure
          putCtrl seq       = System.hPutStr h $ "\x1b[" <> intercalate ";" (show <$> seq) <> "m"
          putFgCtrl         = putCtrl . (38:)
          putBgCtrl         = putCtrl . (48:)
          putFg256Ctrl      = putFgCtrl . (5:) . pure
          -- putBg256Ctrl      = putBgCtrl . (5:) . pure
          putFgTCCtrl r g b = putFgCtrl [2,r,g,b]
          putBgTCCtrl r g b = putBgCtrl [2,r,g,b]
          apply             = \case
              WeightChange    w   -> setSGR $ ANSI.SetConsoleIntensity (convert w)
              ItalicChange    b   -> setSGR $ ANSI.SetItalicized  b
              UnderlineChange u   -> setSGR . ANSI.SetUnderlining $ case u of
                  Nothing              -> ANSI.NoUnderline
                  Just SingleUnderline -> ANSI.SingleUnderline
                  Just DoubleUnderline -> ANSI.DoubleUnderline
              BlinkingChange  b   -> setSGR . ANSI.SetBlinkSpeed $ case b of
                  Nothing        -> ANSI.NoBlink
                  Just SlowBlink -> ANSI.SlowBlink
                  Just FastBlink -> ANSI.RapidBlink
              SwappedChange   b   -> setSGR $ ANSI.SetSwapForegroundBackground b
              FgColorChange   c   -> withJust c $ \case
                  ANSIColor i t   -> setSGR $ ANSI.SetColor ANSI.Foreground (convert i) (convert t)
                  Color256  i     -> putFg256Ctrl i
                  ColorRGB  r g b -> putFgTCCtrl r g b
              BgColorChange   c   -> withJust c $ \case
                  ANSIColor i t   -> setSGR $ ANSI.SetColor ANSI.Background (convert i) (convert t)
                  Color256  i     -> putFg256Ctrl i
                  ColorRGB  r g b -> putBgTCCtrl r g b


-- === Instances === --

instance Default Style where
    def = Style def False def def False def def

instance Convertible ANSIColorIntensity ANSI.ColorIntensity where
    convert = \case Light -> ANSI.Vivid
                    Dark  -> ANSI.Dull

instance Convertible ANSIColorTone ANSI.Color where
    convert = toEnum . fromEnum

instance Convertible WeightType ANSI.ConsoleIntensity where
    convert = \case
        NormalWeight -> ANSI.NormalIntensity
        BoldWeight   -> ANSI.BoldIntensity
        ThinWeight   -> ANSI.FaintIntensity

instance Default WeightType where def = NormalWeight



----------------------
-- === TermText === --
----------------------

-- === Definition === --

data TermText = TermText { _ttlength    :: !Word64
                         , _rootSegment :: !TermTextSegment
                         }

data TermTextSegment = NullSegment
                     | PlainSegment  !Text
                     | StyledSegment ![StyleChange] !TermText
                     | CatSegments   !TermText !TermText

makeLenses ''TermText


-- === Construction === --

plain :: Text -> TermText
plain t = TermText (convert $ Text.length t) (PlainSegment t)


-- === Modification === --

length :: TermText -> Word64
length = view ttlength

-- FIXME[WD]!!!
take :: Word64 -> TermText -> TermText
take _ = id


-- === Outputting === --

hPutStr, hPutStrLn :: MonadIO m => Handle -> TermText -> m ()
hPutStr   h t = liftIO $ State.evalDefT @Style $ hRenderStr h t
hPutStrLn h t = liftIO $ hPutStr h t >> System.hPutChar h '\n'

putStr, putStrLn :: MonadIO m => TermText -> m ()
putStr   = hPutStr   stdout
putStrLn = hPutStrLn stdout

hRenderStr :: (MonadIO m, State.Monad Style m) => Handle -> TermText -> m ()
hRenderStr h txt = case txt ^. rootSegment of
    NullSegment       -> pure ()
    PlainSegment  t   -> liftIO $ Text.hPutStr h t
    StyledSegment s t -> withStyleChanges h s $ hRenderStr h t
    CatSegments   l r -> hRenderStr h l >> hRenderStr h r


-- === Smart styles management === --

class Stylable a where
    cleanStyles :: a -> a
    withStyle   :: [StyleChange] -> a -> a

    default cleanStyles :: (a ~ f a', Stylable a', Functor f) => a -> a
    default withStyle   :: (a ~ f a', Stylable a', Functor f) => [StyleChange] -> a -> a
    cleanStyles = fmap   cleanStyles
    withStyle   = fmap . withStyle

instance Stylable TermText where
    cleanStyles (TermText l s) = go s where
        go = \case StyledSegment _ t -> cleanStyles t
                   CatSegments   a b -> TermText l $ CatSegments (cleanStyles a) (cleanStyles b)
                   t                 -> TermText l t

    withStyle changes t = TermText (length t) $ StyledSegment changes t

styled :: Stylable a => [StyleChange] -> a -> a
styled s = withStyle s . cleanStyles

normal, bold, thin :: StyleChange
normal = WeightChange NormalWeight
bold   = WeightChange BoldWeight
thin   = WeightChange ThinWeight

italic, notItalic :: StyleChange
italic    = ItalicChange True
notItalic = ItalicChange False

underline, doubleUnderline, noUnderline :: StyleChange
underline       = UnderlineChange $ Just SingleUnderline
doubleUnderline = UnderlineChange $ Just DoubleUnderline
noUnderline     = UnderlineChange Nothing

blinking, blinkingFast, notBlinking :: StyleChange
blinking     = BlinkingChange $ Just SlowBlink
blinkingFast = BlinkingChange $ Just FastBlink
notBlinking  = BlinkingChange Nothing

swapped, notSwapped :: StyleChange
swapped    = SwappedChange True
notSwapped = SwappedChange False

fg, bg :: Maybe Color -> StyleChange
fg = FgColorChange
bg = BgColorChange


-- Ansi color constructors

dark, light :: ANSIColorTone -> Maybe Color
dark  = Just . ANSIColor Dark
light = Just . ANSIColor Light

black, red, green, yellow, blue, magenta, cyan, white :: ANSIColorTone
black   = Black
red     = Red
green   = Green
yellow  = Yellow
blue    = Blue
magenta = Magenta
cyan    = Cyan
white   = White


-- 256 color constructors

_256 :: Word8 -> Maybe Color
_256 = Just . Color256

-- truecolor constructors

rgb :: Word8 -> Word8 -> Word8 -> Maybe Color
rgb r g b = Just $ ColorRGB r g b


-- === Instances === --

-- Show
instance Show TermText where show = show . convertTo @Text
deriving instance Show TermTextSegment

-- Monoid
instance Mempty    TermText where mempty = TermText 0 NullSegment
instance Semigroup TermText where
    TermText _ NullSegment <> a = a
    a <> TermText _ NullSegment = a
    a <> b = TermText (length a + length b) $ CatSegments a b

-- Converts
instance IsString TermText where fromString = convert
instance Convertible String   TermText where convert = convertVia @Text
instance Convertible TermText String   where convert = convertVia @Text
instance Convertible Text     TermText where convert = plain
instance Convertible TermText Text     where
    convert = convert . go where
        go :: TermText -> Text.Builder
        go t = case t ^. rootSegment of
            NullSegment       -> mempty
            PlainSegment    t -> convert t
            StyledSegment _ t -> go t
            CatSegments   l r -> go l <> go r

-- Container
type instance Item TermText = Char
