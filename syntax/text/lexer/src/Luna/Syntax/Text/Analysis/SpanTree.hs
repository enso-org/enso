{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Luna.Syntax.Text.Analysis.SpanTree where

import qualified Prelude  as P
import           Prologue hiding (empty, span)

import qualified Data.FingerTree        as FT
import qualified Data.Text32            as Text32
import qualified Luna.Syntax.Text.Lexer as Lexer

import Data.FingerTree    (FingerTree, Measured, measure)
import Data.Text.Position (Delta)
import Data.Text32        (Text32)



------------------
-- === Span === --
------------------

-- === Definition === --

data SpanType
    = TextSpan
    | OffSpan
    | MarkerSpan
    deriving (Show, Eq)

data Span = Span
    { _spanType   :: !SpanType
    , _spanLength :: !Delta
    } deriving (Show)

data SpanGroup = SpanGroup
    { _realLength :: !Delta
    , _viewLength :: !Delta
    } deriving (Show)

makeClassy ''Span
makeLenses ''SpanGroup


-- === Utils === --

textSpan, offSpan, markerSpan :: Delta -> Span
textSpan   = Span TextSpan   ; {-# INLINE textSpan   #-}
offSpan    = Span OffSpan    ; {-# INLINE offSpan    #-}
markerSpan = Span MarkerSpan ; {-# INLINE markerSpan #-}

spanGroup :: Delta -> Delta -> SpanGroup
spanGroup = SpanGroup ; {-# INLINE spanGroup #-}

isSpanType :: SpanType -> Span -> Bool
isSpanType t = (== t) . view spanType ; {-# INLINE isSpanType #-}

isTextSpan, isOffSpan, isMarkerSpan :: Span -> Bool
isTextSpan   = isSpanType TextSpan   ; {-# INLINE isTextSpan   #-}
isOffSpan    = isSpanType OffSpan    ; {-# INLINE isOffSpan    #-}
isMarkerSpan = isSpanType MarkerSpan ; {-# INLINE isMarkerSpan #-}


-- === Conversions === --

instance Convertible Span SpanGroup where
    convert = \case
        Span TextSpan   l -> SpanGroup l l
        Span OffSpan    l -> SpanGroup l l
        Span MarkerSpan l -> SpanGroup l 0
    {-# INLINE convert #-}


-- === Instances === --

instance (a ~ Delta, b ~ Delta) => Convertible SpanGroup  (a,b) where convert (SpanGroup rl ml) = (rl, ml) ; {-# INLINE convert #-}
instance (a ~ Delta, b ~ Delta) => Convertible (a,b) SpanGroup  where convert (rl, ml) = SpanGroup rl ml   ; {-# INLINE convert #-}

instance Mempty    SpanGroup where mempty = SpanGroup mempty mempty ; {-# INLINE mempty #-}
instance Semigroup SpanGroup where SpanGroup rl ml <> SpanGroup rl' ml' = SpanGroup (rl <> rl') (ml <> ml') ; {-# INLINE (<>) #-}
instance P.Monoid  SpanGroup where
    mempty  = mempty ; {-# INLINE mempty  #-}
    mappend = (<>)   ; {-# INLINE mappend #-}



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

spannedText :: Text32 -> Spanned Text32
spannedText t = textSpanned (convert $ Text32.length t) t

splitSpannedText :: Int -> Spanned Text32 -> (Spanned Text32, Spanned Text32)
splitSpannedText i st = over both spannedText $ Text32.splitAt i (st ^. value)

insertIntoSpannedText :: Int -> Text32 -> Spanned Text32 -> Spanned Text32
insertIntoSpannedText i t st = spannedText $ pfx <> t <> sfx where
    (pfx,sfx) = Text32.splitAt i (st ^. value)

initSpannedText :: Spanned Text32 -> Spanned Text32
initSpannedText st = spannedText . Text32.unsafeInit $ st ^. value


-- === Instances === --

instance HasSpan (Spanned a) where span = spanned_span ; {-# INLINE span #-}



----------------------
-- === Spantree === --
----------------------

-- === Definition === --

type    Spantree'  = Spantree ()
newtype Spantree a = Spantree (FingerTree SpanGroup (Spanned a)) deriving (Show, Foldable)
makeLenses ''Spantree


-- === Utils === --

empty :: Spantree a
empty = wrap FT.empty ; {-# INLINE empty #-}

singleton :: Spanned a -> Spantree a
singleton = wrap . FT.singleton ; {-# INLINE singleton #-}

infixl 5 |>
infixr 5 <|
(|>) :: Spantree a -> Spanned  a -> Spantree a
(<|) :: Spanned  a -> Spantree a -> Spantree a
(|>) = flip append ; {-# INLINE (|>) #-}
(<|) = prepend     ; {-# INLINE (<|) #-}

append  :: Spanned a -> Spantree a -> Spantree a
prepend :: Spanned a -> Spantree a -> Spantree a
append  s = wrapped %~ (FT.|> convert' s) ; {-# INLINE append  #-}
prepend s = wrapped %~ (convert' s FT.<|) ; {-# INLINE prepend #-}

split :: (SpanGroup -> Bool) -> Spantree a -> (Spantree a, Spantree a)
split f = over both wrap . FT.split f . unwrap ; {-# INLINE split #-}

splitAtRealOffset :: Delta -> Spantree a -> (Spantree a, Spantree a)
splitAtRealOffset t = split $ (> t) . view realLength ; {-# INLINE splitAtRealOffset #-}

splitAtViewOffset :: Bool -> Delta -> Spantree a -> (Spantree a, Spantree a)
splitAtViewOffset skipMarkers t = split $ cmp . view viewLength where
    cmp = if skipMarkers then (> t) else (>= t)
{-# INLINE splitAtViewOffset #-}

splitFirst :: Spantree a -> Maybe (Spanned a, Spantree a)
splitFirst st = case FT.viewl (unwrap st) of
    FT.EmptyL   -> Nothing
    a FT.:< st' -> Just (a, wrap st')
{-# INLINE splitFirst #-}

splitLast :: Spantree a -> Maybe (Spanned a, Spantree a)
splitLast st = case FT.viewr (unwrap st) of
    FT.EmptyR   -> Nothing
    st' FT.:> a -> Just (a, wrap st')
{-# INLINE splitLast #-}

foldlSpans :: (b -> Spanned a -> b) -> b -> Spantree a -> b
foldlSpans f b = foldl f b . unwrap ; {-# INLINE foldlSpans #-}


-- === Instances === --

-- FIXME[WD]: We should implement FingerTree using modern Haskell (using TypeFamilies) instead of fundeps.
--            We can then drop such ugly hacks as `FingerTree.fmap'`
instance Functor  Spantree where fmap f st = wrap $ FT.fmap' (fmap f) (unwrap st)

instance Measured SpanGroup Span         where measure = convert             ; {-# INLINE measure #-}
instance Measured SpanGroup (Spanned  a) where measure = measure . view span ; {-# INLINE measure #-}
instance Measured SpanGroup (Spantree a) where measure = measure . unwrap    ; {-# INLINE measure #-}

instance Mempty    (Spantree a) where mempty = wrap mempty                 ; {-# INLINE mempty #-}
instance Semigroup (Spantree a) where a <> b = wrap $ unwrap a <> unwrap b ; {-# INLINE (<>)   #-}



------------------------------------
-- === Source text management === --
------------------------------------

-- === Spantree construction === --

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!! FIXME[WD]: GHC PANIC HERE !!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

buildSpanTree :: Text32 -> [Lexer.Token Lexer.Symbol] -> Spantree Text32
buildSpanTree src = \case
    []     -> empty
    (t:ts) -> tailTree & case t ^. Lexer.symbol of
        Lexer.Marker _ -> preprendSpan span $ flip markerSpanned txtSrc
        _              -> preprendSpan span $ flip textSpanned   txtSrc
        where span             = t ^. Lexer.span
              off              = t ^. Lexer.offset
              tailTree         = addOffSpan $ buildSpanTree src'' ts
              addOffSpan       = preprendSpan off $ flip offSpanned offSrc
              (txtSrc, src')   = Text32.splitAt (convert span) src
              (offSrc, src'')  = Text32.splitAt (convert off)  src'
              preprendSpan :: Delta -> (Delta -> Spanned a) -> (Spantree a -> Spantree a)
              preprendSpan d f = if d > 0 then (f d <|) else id
{-# NOINLINE buildSpanTree #-}


-- === View / Real position management === --

viewToRealCursorSplitAfterMarker  ::         Spantree a -> Delta -> (Delta, (Delta, Spantree a, Spantree a))
viewToRealCursorSplitBeforeMarker ::         Spantree a -> Delta -> (Delta, (Delta, Spantree a, Spantree a))
viewToRealCursorSplit             :: Bool -> Spantree a -> Delta -> (Delta, (Delta, Spantree a, Spantree a))
viewToRealCursorSplitAfterMarker       = viewToRealCursorSplit True  ; {-# INLINE viewToRealCursorSplitAfterMarker  #-}
viewToRealCursorSplitBeforeMarker      = viewToRealCursorSplit False ; {-# INLINE viewToRealCursorSplitBeforeMarker #-}
viewToRealCursorSplit skipMarkers st d = (realLen + shift, (shift, pre, post)) where
    (pre, post) = splitAtViewOffset skipMarkers d st
    preSize     = measure pre
    shift       = d - preSize ^. viewLength
    realLen     = preSize ^. realLength
{-# INLINE viewToRealCursorSplit #-}

viewToRealCursorAfterMarker  ::         Spantree a -> Delta -> Delta
viewToRealCursorBeforeMarker ::         Spantree a -> Delta -> Delta
viewToRealCursor             :: Bool -> Spantree a -> Delta -> Delta
viewToRealCursorAfterMarker  = viewToRealCursor True         ; {-# INLINE viewToRealCursorAfterMarker  #-}
viewToRealCursorBeforeMarker = viewToRealCursor False        ; {-# INLINE viewToRealCursorBeforeMarker #-}
viewToRealCursor             = fst .:. viewToRealCursorSplit ; {-# INLINE viewToRealCursor             #-}

viewToRealBlock :: Spantree a -> (Delta, Delta) -> (Delta, Delta)
viewToRealBlock st (left, right) = (len, r' + len - shift) where
    (len, (shift, _, post)) = viewToRealCursorSplitAfterMarker st left
    r' = viewToRealCursorBeforeMarker post (shift + right - left)
{-# INLINE viewToRealBlock #-}


-- === Text modification === --

insertText             ::         Delta -> Text32 -> Spantree Text32 -> Spantree Text32
insertTextBeforeMarker ::         Delta -> Text32 -> Spantree Text32 -> Spantree Text32
insertTextByMarker     :: Bool -> Delta -> Text32 -> Spantree Text32 -> Spantree Text32
insertText             = insertTextByMarker True  ; {-# INLINE insertText             #-}
insertTextBeforeMarker = insertTextByMarker False ; {-# INLINE insertTextBeforeMarker #-}
insertTextByMarker skipMarkers d t st = pre <> if betweenSegments then simplePost else complexPost where
    (shift, pre, post) = snd $ viewToRealCursorSplit skipMarkers st d
    betweenSegments    = shift == mempty
    simplePost         = spannedText t <| post
    complexPost        = case splitFirst post of
        Just (head, tail) -> insertIntoSpannedText (convert shift) t head <| tail
        Nothing           -> singleton $ spannedText t
{-# INLINE insertTextByMarker #-}

breakLine :: Delta -> Spantree Text32 -> Spantree Text32
breakLine = flip (insertTextByMarker False) "\n" ; {-# INLINE breakLine #-}

