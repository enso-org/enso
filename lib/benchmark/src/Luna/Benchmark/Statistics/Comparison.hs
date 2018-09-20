{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Benchmark.Statistics.Comparison where

import Prologue

import qualified Data.Map                  as Map
import qualified Data.Set                  as Set
import qualified Luna.Benchmark.Statistics as Statistics

import Data.Map                                     (Map)
import Luna.Benchmark.Location                      (SrcLoc)
import Luna.Benchmark.Statistics                    (Statistics)
import Luna.Benchmark.Statistics.Comparison.Orphans ()
import Perf                                         (Cycle)



------------------------
-- === Comparison === --
------------------------

-- === Definition === --

newtype Comparison a = Comparison
    { _value :: a
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''Comparison


-- === API === --

renderComparisons :: (StyledShow PrettyShowStyle a, Show a) => [Comparison a] -> Text
renderComparisons comps = intercalate ", " $ prettyShow <$> comps


-- === Instances === --

instance (Default a) => Default (Comparison a) where
    def = Comparison def

instance (Show a) => StyledShow PrettyShowStyle (Comparison a) where
    styledShow _ (Comparison val) = "(" <> convert (show val) <> ")"



------------------------------
-- === ComparisonResult === --
------------------------------

-- === Definition === --

data ComparisonResult = ComparisonResult
    { _locName            :: !Text
    , _sourceLocs         :: ![SrcLoc]
    , _maxTimeComp        :: !(Double, [Comparison Double])
    , _minTimeComp        :: !(Double, [Comparison Double])
    , _avgTimeComp        :: !(Double, [Comparison Double])
    , _stdTimeComp        :: !(Double, [Comparison Double])
    , _maxTicksComp       :: !(Cycle, [Comparison Cycle])
    , _minTicksComp       :: !(Cycle, [Comparison Cycle])
    , _avgTicksComp       :: !(Cycle, [Comparison Cycle])
    , _stdTicksComp       :: !(Double, [Comparison Double])
    , _maxLiveBytesComp   :: !(Int64, [Comparison Int64])
    , _minLiveBytesComp   :: !(Int64, [Comparison Int64])
    , _avgLiveBytesComp   :: !(Int64, [Comparison Int64])
    , _stdLiveBytesComp   :: !(Double, [Comparison Double])
    , _maxActualBytesComp :: !(Int64, [Comparison Int64])
    , _minActualBytesComp :: !(Int64, [Comparison Int64])
    , _avgActualBytesComp :: !(Int64, [Comparison Int64])
    , _stdActualBytesComp :: !(Double, [Comparison Double])
    , _maxMaxBytesComp    :: !(Int64, [Comparison Int64])
    , _minMaxBytesComp    :: !(Int64, [Comparison Int64])
    , _avgMaxBytesComp    :: !(Int64, [Comparison Int64])
    , _stdMaxBytesComp    :: !(Double, [Comparison Double])
    , _maxNumGCsComp      :: !(Int64, [Comparison Int64])
    , _minNumGCsComp      :: !(Int64, [Comparison Int64])
    , _avgNumGCsComp      :: !(Int64, [Comparison Int64])
    , _stdNumGCsComp      :: !(Double, [Comparison Double])
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''ComparisonResult


-- === API === --

compare :: MonadIO m => Map Text Statistics -> [Map Text Statistics]
    -> m [ComparisonResult]
compare current history = for (Map.elems current) $ \loc -> do
    let key         = loc ^. Statistics.locationName
        histResults = catMaybes $ (Map.lookup key) <$> history
        result      = def @ComparisonResult
            & locName     .~ (loc ^. Statistics.locationName)
            & sourceLocs  .~ (Set.toList $ loc ^. Statistics.sourceLocations )
            & maxTimeComp .~
                ( loc ^. Statistics.timeInfo . Statistics.maxTime
                , compareField loc (Statistics.timeInfo . Statistics.maxTime)
                    <$> histResults )
            & minTimeComp .~
                ( loc ^. Statistics.timeInfo . Statistics.minTime
                , compareField loc (Statistics.timeInfo . Statistics.minTime)
                    <$> histResults)
            & avgTimeComp .~
                ( loc ^. Statistics.timeInfo . Statistics.avgTime
                , compareField loc (Statistics.timeInfo . Statistics.avgTime)
                    <$> histResults)
            & stdTimeComp .~
                ( loc ^. Statistics.timeInfo . Statistics.stdTime
                , compareField loc (Statistics.timeInfo . Statistics.stdTime)
                    <$> histResults)
            & maxTicksComp .~
                ( loc ^. Statistics.tickInfo . Statistics.maxTicks
                , compareField loc (Statistics.tickInfo . Statistics.maxTicks)
                    <$> histResults)
            & minTicksComp .~
                ( loc ^. Statistics.tickInfo . Statistics.minTicks
                , compareField loc (Statistics.tickInfo . Statistics.minTicks)
                    <$> histResults)
            & avgTicksComp .~
                ( loc ^. Statistics.tickInfo . Statistics.avgTicks
                , compareField loc (Statistics.tickInfo . Statistics.avgTicks)
                    <$> histResults)
            & stdTicksComp .~
                ( loc ^. Statistics.tickInfo . Statistics.stdTicks
                , compareField loc (Statistics.tickInfo . Statistics.stdTicks)
                    <$> histResults)
            & maxLiveBytesComp .~
                (loc ^. Statistics.memInfo . Statistics.maxMem
                    . Statistics.liveBytes
                , compareField loc (Statistics.memInfo . Statistics.maxMem
                        . Statistics.liveBytes)
                    <$> histResults)
            & minLiveBytesComp .~
                (loc ^. Statistics.memInfo . Statistics.minMem
                    . Statistics.liveBytes
                , compareField loc (Statistics.memInfo . Statistics.minMem
                        . Statistics.liveBytes)
                    <$> histResults)
            & avgLiveBytesComp .~
                (loc ^. Statistics.memInfo . Statistics.avgMem
                    . Statistics.liveBytes
                , compareField loc (Statistics.memInfo . Statistics.avgMem
                        . Statistics.liveBytes)
                    <$> histResults)
            & stdLiveBytesComp .~
                (loc ^. Statistics.memInfo . Statistics.stdMem
                    . Statistics.liveBytes
                , compareField loc (Statistics.memInfo . Statistics.stdMem
                        . Statistics.liveBytes)
                    <$> histResults)
            & maxActualBytesComp .~
                (loc ^. Statistics.memInfo . Statistics.maxMem
                    . Statistics.actualBytes
                , compareField loc (Statistics.memInfo . Statistics.maxMem
                        . Statistics.actualBytes)
                    <$> histResults)
            & minLiveBytesComp .~
                (loc ^. Statistics.memInfo . Statistics.minMem
                    . Statistics.actualBytes
                , compareField loc (Statistics.memInfo . Statistics.minMem
                        . Statistics.actualBytes)
                    <$> histResults)
            & avgLiveBytesComp .~
                (loc ^. Statistics.memInfo . Statistics.avgMem
                    . Statistics.actualBytes
                , compareField loc (Statistics.memInfo . Statistics.avgMem
                        . Statistics.actualBytes)
                    <$> histResults)
            & stdLiveBytesComp .~
                (loc ^. Statistics.memInfo . Statistics.stdMem
                    . Statistics.actualBytes
                , compareField loc (Statistics.memInfo . Statistics.stdMem
                        . Statistics.actualBytes)
                    <$> histResults)
            & maxMaxBytesComp .~
                (loc ^. Statistics.memInfo . Statistics.maxMem
                    . Statistics.maxBytes
                , compareField loc (Statistics.memInfo . Statistics.maxMem
                        . Statistics.maxBytes)
                    <$> histResults)
            & minMaxBytesComp .~
                (loc ^. Statistics.memInfo . Statistics.minMem
                    . Statistics.maxBytes
                , compareField loc (Statistics.memInfo . Statistics.minMem
                        . Statistics.maxBytes)
                    <$> histResults)
            & avgMaxBytesComp .~
                (loc ^. Statistics.memInfo . Statistics.avgMem
                    . Statistics.maxBytes
                , compareField loc (Statistics.memInfo . Statistics.avgMem
                        . Statistics.maxBytes)
                    <$> histResults)
            & stdMaxBytesComp .~
                (loc ^. Statistics.memInfo . Statistics.stdMem
                    . Statistics.maxBytes
                , compareField loc (Statistics.memInfo . Statistics.stdMem
                        . Statistics.maxBytes)
                    <$> histResults)
            & maxNumGCsComp .~
                (loc ^. Statistics.memInfo . Statistics.maxMem
                    . Statistics.numGCs
                , compareField loc (Statistics.memInfo . Statistics.maxMem
                        . Statistics.numGCs)
                    <$> histResults)
            & minNumGCsComp .~
                (loc ^. Statistics.memInfo . Statistics.minMem
                    . Statistics.numGCs
                , compareField loc (Statistics.memInfo . Statistics.minMem
                        . Statistics.numGCs)
                    <$> histResults)
            & avgNumGCsComp .~
                (loc ^. Statistics.memInfo . Statistics.avgMem
                    . Statistics.numGCs
                , compareField loc (Statistics.memInfo . Statistics.avgMem
                        . Statistics.numGCs)
                    <$> histResults)
            & stdNumGCsComp .~
                (loc ^. Statistics.memInfo . Statistics.stdMem
                    . Statistics.numGCs
                , compareField loc (Statistics.memInfo . Statistics.stdMem
                        . Statistics.numGCs)
                    <$> histResults)
    pure result

compareField :: (Num a, Ord a) => Statistics -> Lens' Statistics a -> Statistics
     -> Comparison a
compareField current accessor old = let
    currVal = current ^. accessor
    histVal = old     ^. accessor
    delta   = currVal - histVal
    in Comparison delta

renderLocations :: [SrcLoc] -> Text
renderLocations locs = intercalate ("\n" <> ind) $ prettyShow <$> locs


-- === Instances === --

instance Default ComparisonResult

ind :: Text
ind = convert $ replicate (4 :: Int) ' '

delta :: Text
delta = " delta "

instance StyledShow PrettyShowStyle ComparisonResult where
    styledShow _ compResult
        =  "== Location: " <> compResult ^. locName <> "\n"
        <> ind <> renderLocations (compResult ^. sourceLocs) <> "\n\n"
        <> ind <> "-- Time:\n"
        <> ind <> "Max Time: "
            <> convert (show (compResult ^. maxTimeComp . _1)) <> delta
            <> renderComparisons (compResult ^. maxTimeComp . _2) <> "\n"
        <> ind <> "Min Time: "
            <> convert (show (compResult ^. minTimeComp . _1)) <> delta
            <> renderComparisons (compResult ^. minTimeComp . _2) <> "\n"
        <> ind <> "Mean Time: "
            <> convert (show (compResult ^. avgTimeComp . _1)) <> delta
            <> renderComparisons (compResult ^. avgTimeComp . _2) <> "\n"
        <> ind <> "Standard Deviation Time: "
            <> convert (show (compResult ^. stdTimeComp . _1)) <> delta
            <> renderComparisons (compResult ^. stdTimeComp . _2) <> "\n\n"
        <> ind <> "-- Ticks:\n"
        <> ind <> "Max Ticks: "
            <> convert (show (compResult ^. maxTicksComp . _1)) <> delta
            <> renderComparisons (compResult ^. maxTicksComp . _2) <> "\n"
        <> ind <> "Min Ticks: "
            <> convert (show (compResult ^. minTicksComp . _1)) <> delta
            <> renderComparisons (compResult ^. minTicksComp . _2) <> "\n"
        <> ind <> "Mean Ticks: "
            <> convert (show (compResult ^. avgTicksComp . _1)) <> delta
            <> renderComparisons (compResult ^. avgTicksComp . _2) <> "\n"
        <> ind <> "Standard Deviation Ticks: "
            <> convert (show (compResult ^. stdTicksComp . _1)) <> delta
            <> renderComparisons (compResult ^. stdTicksComp . _2) <> "\n\n"
        <> ind <> "-- Live Bytes:\n"
        <> ind <> "Max Bytes: "
            <> convert (show (compResult ^. maxLiveBytesComp . _1)) <> delta
            <> renderComparisons (compResult ^. stdTicksComp . _2) <> "\n"
        <> ind <> "Min Bytes: "
            <> convert (show (compResult ^. minLiveBytesComp . _1)) <> delta
            <> renderComparisons (compResult ^. minLiveBytesComp . _2) <> "\n"
        <> ind <> "Mean Bytes: "
            <> convert (show (compResult ^. avgLiveBytesComp . _1)) <> delta
            <> renderComparisons (compResult ^. avgLiveBytesComp . _2) <> "\n"
        <> ind <> "Standard Deviation Bytes: "
            <> convert (show (compResult ^. stdLiveBytesComp . _1)) <> delta
            <> renderComparisons (compResult ^. stdLiveBytesComp . _2) <> "\n\n"
        <> ind <> "-- Actual Bytes:\n"
        <> ind <> "Max Bytes: "
            <> convert (show (compResult ^. maxActualBytesComp . _1)) <> delta
            <> renderComparisons (compResult ^. maxActualBytesComp . _2) <> "\n"
        <> ind <> "Min Bytes: "
            <> convert (show (compResult ^. minActualBytesComp . _1)) <> delta
            <> renderComparisons (compResult ^. minActualBytesComp . _2) <> "\n"
        <> ind <> "Mean Bytes: "
            <> convert (show (compResult ^. avgActualBytesComp . _1)) <> delta
            <> renderComparisons (compResult ^. avgActualBytesComp . _2) <> "\n"
        <> ind <> "Standard Deviation Bytes: "
            <> convert (show (compResult ^. stdActualBytesComp . _1)) <> delta
            <> renderComparisons (compResult ^. stdActualBytesComp . _2) <> "\n\n"
        <> ind <> "-- Max Bytes:\n"
        <> ind <> "Max Bytes: "
            <> convert (show (compResult ^. maxMaxBytesComp . _1)) <> delta
            <> renderComparisons (compResult ^. maxMaxBytesComp . _2) <> "\n"
        <> ind <> "Min Bytes: "
            <> convert (show (compResult ^. minMaxBytesComp . _1)) <> delta
            <> renderComparisons (compResult ^. minMaxBytesComp . _2) <> "\n"
        <> ind <> "Mean Bytes: "
            <> convert (show (compResult ^. avgMaxBytesComp . _1)) <> delta
            <> renderComparisons (compResult ^. avgMaxBytesComp . _2) <> "\n"
        <> ind <> "Standard Deviation Bytes: "
            <> convert (show (compResult ^. stdMaxBytesComp . _1)) <> delta
            <> renderComparisons (compResult ^. stdMaxBytesComp . _2) <> "\n\n"
        <> ind <> "-- Number of GCs:\n"
        <> ind <> "Max GCs: "
            <> convert (show (compResult ^. maxNumGCsComp . _1)) <> delta
            <> renderComparisons (compResult ^. maxNumGCsComp . _2) <> "\n"
        <> ind <> "Min GCs: "
            <> convert (show (compResult ^. minNumGCsComp . _1)) <> delta
            <> renderComparisons (compResult ^. minNumGCsComp . _2) <> "\n"
        <> ind <> "Mean GCs: "
            <> convert (show (compResult ^. avgNumGCsComp . _1)) <> delta
            <> renderComparisons (compResult ^. avgNumGCsComp . _2) <> "\n"
        <> ind <> "Standard Deviation GCs: "
            <> convert (show (compResult ^. stdNumGCsComp . _1)) <> delta
            <> renderComparisons (compResult ^. stdNumGCsComp . _2)

