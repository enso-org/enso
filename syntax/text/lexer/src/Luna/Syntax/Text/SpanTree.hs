{-# LANGUAGE OverloadedStrings #-}

module Luna.Syntax.Text.SpanTree where

import Prologue hiding (span, (|>), (<|), empty)

import qualified Prelude                as P
import qualified Data.FingerTree        as FT
import           Data.FingerTree        (Measured, FingerTree, takeUntil, dropUntil, measure)
import           Data.Text.Position     (Delta)
import           Data.VectorText        (VectorText)
import qualified Data.VectorText        as VectorText
import qualified Luna.Syntax.Text.Lexer as Lexer



-- vvv Missing instances vvv --

instance Measured v a => Semigroup (FingerTree v a) where (<>) = (FT.><)

-- ^^^ ----------------- ^^^ --

------------------
-- === Span === --
------------------

-- === Definition === --

data SpanType = TextSpan
              | OffSpan
              | MarkerSpan
              deriving (Show, Eq)

data Span = Span { _spanType   :: !SpanType
                 , _spanLength :: !Delta
                 } deriving (Show)

data SpanGroup = SpanGroup { _realLength :: !Delta
                           , _viewLength :: !Delta
                           } deriving (Show)

makeClassy ''Span
makeLenses ''SpanGroup


-- === Utils === --

textSpan, offSpan, markerSpan :: Delta -> Span
textSpan   = Span TextSpan
offSpan    = Span OffSpan
markerSpan = Span MarkerSpan

spanGroup :: Delta -> Delta -> SpanGroup
spanGroup = SpanGroup

isSpanType :: SpanType -> Span -> Bool
isSpanType t = (== t) . view spanType

isTextSpan, isOffSpan, isMarkerSpan :: Span -> Bool
isTextSpan   = isSpanType TextSpan
isOffSpan    = isSpanType OffSpan
isMarkerSpan = isSpanType MarkerSpan


-- === Conversions === --

instance Convertible Span SpanGroup where
    convert = \case
        Span TextSpan   l -> SpanGroup l l
        Span OffSpan    l -> SpanGroup l l
        Span MarkerSpan l -> SpanGroup l 0


-- === Instances === --

instance (a ~ Delta, b ~ Delta) => Convertible SpanGroup  (a,b) where convert (SpanGroup rl ml) = (rl, ml)
instance (a ~ Delta, b ~ Delta) => Convertible (a,b) SpanGroup  where convert (rl, ml) = SpanGroup rl ml

instance Mempty    SpanGroup where mempty = SpanGroup mempty mempty
instance Semigroup SpanGroup where SpanGroup rl ml <> SpanGroup rl' ml' = SpanGroup (rl <> rl') (ml <> ml')
instance P.Monoid  SpanGroup where
    mempty  = mempty
    mappend = (<>)



----------------------
-- === Spanned === --
----------------------

-- === Definition === --

data Spanned a = Spanned { __span :: !Span
                         , _value :: !a
                         } deriving (Show, Functor, Foldable)
makeLenses ''Spanned


-- === Utils === --

textSpanned, offSpanned, markerSpanned :: Delta -> a -> Spanned a
textSpanned   = Spanned . textSpan
offSpanned    = Spanned . offSpan
markerSpanned = Spanned . markerSpan

spannedText :: VectorText -> Spanned VectorText
spannedText t = textSpanned (convert $ VectorText.length t) t

splitSpannedText :: Int -> Spanned VectorText -> (Spanned VectorText, Spanned VectorText)
splitSpannedText i st = over both spannedText $ VectorText.splitAt i (st ^. value)

insertIntoSpannedText :: Int -> VectorText -> Spanned VectorText -> Spanned VectorText
insertIntoSpannedText i t st = spannedText $ pfx <> t <> sfx where
    (pfx,sfx) = VectorText.splitAt i (st ^. value)

initSpannedText :: Spanned VectorText -> Spanned VectorText
initSpannedText st = spannedText . VectorText.init $ st ^. value


-- === Instances === --

instance HasSpan (Spanned a) where span = spanned_span



----------------------
-- === Spantree === --
----------------------

-- === Definition === --

type    Spantree'  = Spantree ()
newtype Spantree a = Spantree (FingerTree SpanGroup (Spanned a)) deriving (Show, Foldable)
makeLenses ''Spantree


-- === Utils === --

empty :: Spantree a
empty = wrap FT.empty

singleton :: Spanned a -> Spantree a
singleton = wrap . FT.singleton

infixl 5 |>
infixr 5 <|
(|>) :: Spantree a -> Spanned  a -> Spantree a
(<|) :: Spanned  a -> Spantree a -> Spantree a
(|>) = flip append
(<|) = prepend

append  :: Spanned a -> Spantree a -> Spantree a
prepend :: Spanned a -> Spantree a -> Spantree a
append  span = wrapped %~ (FT.|> convert' span)
prepend span = wrapped %~ (convert' span FT.<|)

split :: (SpanGroup -> Bool) -> Spantree a -> (Spantree a, Spantree a)
split f = over both wrap . FT.split f . unwrap

splitAtRealOffset :: Delta -> Spantree a -> (Spantree a, Spantree a)
splitAtRealOffset t = split $ (> t) . view realLength

splitAtViewOffset :: Bool -> Delta -> Spantree a -> (Spantree a, Spantree a)
splitAtViewOffset skipMarkers t = split $ cmp . view viewLength where
    cmp = if skipMarkers then (> t) else (>= t)

splitFirst :: Spantree a -> Maybe (Spanned a, Spantree a)
splitFirst st = case FT.viewl (unwrap st) of
    FT.EmptyL   -> Nothing
    a FT.:< st' -> Just (a, wrap st')

splitLast :: Spantree a -> Maybe (Spanned a, Spantree a)
splitLast st = case FT.viewr (unwrap st) of
    FT.EmptyR   -> Nothing
    st' FT.:> a -> Just (a, wrap st')

foldlSpans :: (b -> Spanned a -> b) -> b -> Spantree a -> b
foldlSpans f b = foldl f b . unwrap


-- === Instances === --

-- FIXME[WD]: We should implement FingerTree using modern Haskell (using TypeFamilies) instead of fundeps.
--            We can then drop such ugly hacks as `FingerTree.fmap'`
instance Functor  Spantree where fmap f st = wrap $ FT.fmap' (fmap f) (unwrap st)

instance Measured SpanGroup Span         where measure = convert
instance Measured SpanGroup (Spanned  a) where measure = measure . view span
instance Measured SpanGroup (Spantree a) where measure = measure . unwrap

instance Mempty    (Spantree a) where mempty = wrap mempty
instance Semigroup (Spantree a) where a <> b = wrap $ unwrap a <> unwrap b



------------------------------------
-- === Source text management === --
------------------------------------

-- === Spantree construction === --

buildSpanTree :: VectorText -> [Lexer.LexerToken (Lexer.Symbol a)] -> Spantree VectorText
buildSpanTree src = \case
    []     -> empty
    (t:ts) -> tailTree & case t ^. Lexer.element of
        Lexer.Marker _ -> preprendSpan span $ flip markerSpanned txtSrc
        _              -> preprendSpan span $ flip textSpanned   txtSrc
        where span             = t ^. Lexer.span
              off              = t ^. Lexer.offset
              tailTree         = addOffSpan $ buildSpanTree src'' ts
              addOffSpan       = preprendSpan off $ flip offSpanned offSrc
              (txtSrc, src')   = VectorText.splitAt (convert span) src
              (offSrc, src'')  = VectorText.splitAt (convert off)  src'
              preprendSpan d f = if d > 0 then (f d <|) else id


-- === View / Real position management === --

viewToRealCursorSplitAfterMarker  ::         Spantree a -> Delta -> (Delta, (Delta, Spantree a, Spantree a))
viewToRealCursorSplitBeforeMarker ::         Spantree a -> Delta -> (Delta, (Delta, Spantree a, Spantree a))
viewToRealCursorSplit             :: Bool -> Spantree a -> Delta -> (Delta, (Delta, Spantree a, Spantree a))
viewToRealCursorSplitAfterMarker       = viewToRealCursorSplit True
viewToRealCursorSplitBeforeMarker      = viewToRealCursorSplit False
viewToRealCursorSplit skipMarkers st d = (realLen + shift, (shift, pre, post)) where
    (pre, post) = splitAtViewOffset skipMarkers d st
    preSize     = measure pre
    shift       = d - preSize ^. viewLength
    realLen     = preSize ^. realLength

viewToRealCursorAfterMarker  ::         Spantree a -> Delta -> Delta
viewToRealCursorBeforeMarker ::         Spantree a -> Delta -> Delta
viewToRealCursor             :: Bool -> Spantree a -> Delta -> Delta
viewToRealCursorAfterMarker  = viewToRealCursor True
viewToRealCursorBeforeMarker = viewToRealCursor False
viewToRealCursor = fst .:. viewToRealCursorSplit

viewToRealBlock :: Spantree a -> (Delta, Delta) -> (Delta, Delta)
viewToRealBlock st (left, right) = (len, r' + len - shift) where
    (len, (shift, pre, post)) = viewToRealCursorSplitAfterMarker st left
    r' = viewToRealCursorBeforeMarker post (shift + right - left)


-- === Text modification === --

insertText             ::         Delta -> VectorText -> Spantree VectorText -> Spantree VectorText
insertTextBeforeMarker ::         Delta -> VectorText -> Spantree VectorText -> Spantree VectorText
insertTextByMarker     :: Bool -> Delta -> VectorText -> Spantree VectorText -> Spantree VectorText
insertText             = insertTextByMarker True
insertTextBeforeMarker = insertTextByMarker False
insertTextByMarker skipMarkers d t st = pre <> if betweenSegments then simplePost else complexPost where
    (shift, pre, post) = snd $ viewToRealCursorSplit skipMarkers st d
    betweenSegments    = shift == mempty
    simplePost         = spannedText t <| post
    complexPost        = case splitFirst post of
        Just (head, tail) -> insertIntoSpannedText (convert shift) t head <| tail
        Nothing           -> singleton $ spannedText t

breakLine :: Delta -> Spantree VectorText -> Spantree VectorText
breakLine = flip (insertTextByMarker False) "\n"
