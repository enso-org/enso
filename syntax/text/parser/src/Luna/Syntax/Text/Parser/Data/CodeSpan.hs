{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoStrict                  #-}
{-# LANGUAGE NoStrictData              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Text.Parser.Data.CodeSpan where

import Prologue hiding (Span, String, Type, length, span)

import qualified Control.Monad.State.Layered        as State
import qualified Data.Generics.Traversable.Deriving as GTraversable
import qualified Data.Text.Span                     as Span
import qualified Foreign.Storable.Deriving          as Storable
import qualified Luna.IR                            as IR
import qualified Luna.IR.Layer                      as Layer
import qualified Data.Graph.Fold.Class              as FoldClass
import qualified Data.Graph.Fold.Scoped             as Fold
import qualified Data.Graph.Fold.Deep               as Fold
import qualified Data.Graph.Fold.Partition          as Fold
import qualified Data.Graph.Store.Buffer            as Buffer
import qualified Data.Graph.Store.Size.Discovery    as Buffer


import Data.Text.Position (Delta)
import Data.Text.Span     (LeftSpacedSpan, length, offset)
import Luna.IR.Layer      (Layer)
import Type.Any           (AnyType)



------------------------
-- === CodeOffset === --
------------------------

-- === Definition === --

data CodeOffset = CodeOffset
    { _realOffset :: Delta
    , _viewOffset :: Delta
    } deriving (Show)
makeLenses ''CodeOffset


-- === Instances === --

instance Mempty    CodeOffset where mempty = CodeOffset mempty mempty
instance Semigroup CodeOffset where CodeOffset r v <> CodeOffset r' v' = CodeOffset (r <> r') (v <> v')



----------------------
-- === CodeSpan === --
----------------------

-- === Definition === --

data CodeSpan = CodeSpan
    { _realSpan :: !LeftSpacedSpan
    , _viewSpan :: !LeftSpacedSpan
    } deriving (Show, Generic)
makeLenses          ''CodeSpan
Storable.derive     ''CodeSpan
GTraversable.derive ''CodeSpan


-- === Utils === --

mkRealSpan :: LeftSpacedSpan -> CodeSpan
mkRealSpan s = CodeSpan s s ; {-# INLINE mkRealSpan #-}

mkPhantomSpan :: LeftSpacedSpan -> CodeSpan
mkPhantomSpan s = CodeSpan s (s & Span.length .~ mempty) ; {-# INLINE mkPhantomSpan #-}

dropOffset :: CodeSpan -> CodeSpan
dropOffset cs = cs & realSpan . offset .~ mempty
                   & viewSpan . offset .~ mempty
{-# INLINE dropOffset #-}

dropLength :: CodeSpan -> CodeSpan
dropLength cs = cs & realSpan . length .~ mempty
                   & viewSpan . length .~ mempty
{-# INLINE dropLength #-}

asOffsetSpan :: CodeSpan -> CodeSpan
asOffsetSpan cs = cs & realSpan %~ Span.asOffsetSpan
                     & viewSpan %~ Span.asOffsetSpan
{-# INLINE asOffsetSpan #-}

asCodeOffset :: CodeSpan -> CodeOffset
asCodeOffset = extractCodeOffset . asOffsetSpan
{-# INLINE asCodeOffset #-}

extractCodeOffset :: CodeSpan -> CodeOffset
extractCodeOffset cs = CodeOffset (cs ^. realSpan . Span.offset) (cs ^. viewSpan . Span.offset)
{-# INLINE extractCodeOffset #-}

-- TODO: remove ? use <> instead?
concat :: CodeSpan -> CodeSpan -> CodeSpan
concat (CodeSpan r v) (CodeSpan r' v') = CodeSpan (Span.concat r r') (Span.concat v v')
-- {-# INLINE concat #-}


-- === Instances === --

instance Mempty CodeSpan where
    mempty = CodeSpan mempty mempty

instance Semigroup CodeSpan where
    CodeSpan r v <> CodeSpan r' v' = CodeSpan (r <> r') (v <> v')



----------------------------
-- === CodeSpan layer === --
----------------------------

instance Layer CodeSpan where
    type Cons  CodeSpan = Layer.Simple CodeSpan
    manager = Layer.customStaticManager (pure $ Layer.Simple mempty)

instance Monad m => FoldClass.Builder (Fold.Scoped (Fold.Deep (Fold.Discovery a))) m CodeSpan
instance Monad m => FoldClass.Builder Buffer.CopyInitialization2 m CodeSpan
instance Monad m => FoldClass.Builder Buffer.CopyInitialization  m CodeSpan
instance Monad m => FoldClass.Builder Buffer.Discovery  m CodeSpan


-- initCodeSpan :: Req m '[Writer // Layer // Abstract (Elem t) // CodeSpan] => Listener (New // Elem t) m
-- initCodeSpan = listener $ \(a,_) -> putLayer @CodeSpan a mempty
-- makePass 'initCodeSpan

-- init :: MonadPassManager m => m ()
-- init = addElemEventListener @CodeSpan initCodeSpanPass


-- -- === Utils === --

-- type AbsSpanReq m = Req m '[ Reader // Layer // AnyExpr     // '[Model, Succs, CodeSpan]
--                            , Reader // Layer // AnyExprLink // Model
--                            ]

-- absSpan :: forall m. AbsSpanReq m => SomeExpr -> m CodeSpan
-- absSpan a = (<>) <$> getParentSpan a <*> getLayer @CodeSpan a where

--     spanUntil :: SomeExpr -> SomeExpr -> m CodeSpan
--     spanUntil el current = do
--         els <- readInputSources current
--         let preds = takeWhile (/= el) els
--         predsSpans <- getLayer @CodeSpan <$>= preds
--         parentSpan <- getParentSpan current
--         return $ parentSpan <> mconcat (asOffsetSpan <$> predsSpans)

--     getParentSpan :: SomeExpr -> m CodeSpan
--     getParentSpan a = getParent a >>= \case
--         Nothing -> return mempty
--         Just p  -> (<>) <$> (dropLength <$> getLayer @CodeSpan p) <*> spanUntil a p


-- type FirstMatchReq m = (Req m '[ Reader // Layer // AnyExpr     // '[Model, CodeSpan]
--                               , Reader // Layer // AnyExprLink // Model
--                               ], MonadIO m)

-- findAstBySpan :: FirstMatchReq m => (CodeOffset -> Bool) -> (CodeOffset -> Bool) -> SomeExpr -> m (Maybe (CodeOffset, SomeExpr))
-- findAstBySpan leftCheck rightCheck expr = go mempty [expr] where
--     go s []     = return Nothing
--     go s (t:ts) = do
--         tcs <- getLayer @CodeSpan t
--         let tcsLen   = asCodeOffset tcs
--             tcsOff   = s <> extractCodeOffset (dropLength tcs)
--             totalLen = s <> tcsLen
--             current  = return $ Just (s,t)
--         if | leftCheck  tcsOff   -> current
--            | rightCheck totalLen -> do
--                ins <- readInputSources t
--                if null ins then current else go tcsOff ins
--            | otherwise -> go totalLen ts

-- findAstByViewSpan, findAstByRealSpan :: FirstMatchReq m => (Delta -> Bool) -> (Delta -> Bool) -> SomeExpr -> m (Maybe (CodeOffset, SomeExpr))
-- findAstByViewSpan leftCheck rightCheck = findAstBySpan (leftCheck . view viewOffset) (rightCheck . view viewOffset)
-- findAstByRealSpan leftCheck rightCheck = findAstBySpan (leftCheck . view realOffset) (rightCheck . view realOffset)

-- splitAtView, splitAtReal :: FirstMatchReq m => Delta -> SomeExpr -> m (Maybe (CodeOffset, SomeExpr))
-- splitAtView d = findAstByViewSpan (>= d) (> d)
-- splitAtReal d = findAstByRealSpan (>= d) (> d)

-- viewToRealOffset_ :: FirstMatchReq m => Delta -> SomeExpr -> m  Delta
-- viewToRealOffset  :: FirstMatchReq m => Delta -> SomeExpr -> m (Maybe SomeExpr, Delta)
-- viewToRealOffset' :: FirstMatchReq m => Delta -> SomeExpr -> m (Maybe (CodeOffset, SomeExpr), Delta)
-- viewToRealOffset_ d a = snd <$> viewToRealOffset d a
-- viewToRealOffset  d a = (_1 %~ fmap snd) <$> viewToRealOffset' d a
-- viewToRealOffset' d a = splitAtView d a >>= \case
--     Nothing -> (Nothing,) . Span.measure . view realSpan <$> getLayer @CodeSpan a
--     t@(Just (leftSpan, _)) -> return (t, realOff) where
--         leftViewOff = leftSpan ^. viewOffset
--         offDiff     = d - leftViewOff
--         realOff     = leftSpan ^. realOffset + offDiff

-- viewToRealOffsetWithMarkers_ :: FirstMatchReq m => Delta -> SomeExpr -> m Delta
-- viewToRealOffsetWithMarkers  :: FirstMatchReq m => Delta -> SomeExpr -> m (Maybe SomeExpr, Delta)
-- viewToRealOffsetWithMarkers_ = fmap3 snd viewToRealOffsetWithMarkers
-- viewToRealOffsetWithMarkers d a = do
--     (es, realOff) <- viewToRealOffset' d a
--     case es of
--         Nothing -> return (Nothing, realOff)
--         Just (leftSpan, expr) -> do
--             (expr', diff) <- matchExpr expr $ \case
--                 Marked lm la -> do
--                     exprSpan <- getLayer @CodeSpan expr
--                     let justBeforeMarker = d == (leftSpan ^. viewOffset) + exprSpan ^. (viewSpan . Span.offset)
--                     if justBeforeMarker then do
--                         a     <- readSource la
--                         m     <- readSource lm
--                         mSpan <- getLayer @CodeSpan m
--                         return (a, Span.measure $ mSpan ^. realSpan)
--                     else return (expr, mempty)
--                 _ -> return (expr, mempty)
--             return (Just expr', realOff + diff)




---------------------------
-- === CodeSpanRange === --
---------------------------

-- | Parser state used to keep the current source range covered by code spans.
newtype CodeSpanRange = CodeSpanRange Delta deriving (Show, Default, Mempty)
makeLenses ''CodeSpanRange

instance Convertible Delta CodeSpanRange where convert = wrap ; {-# INLINE convert #-}

