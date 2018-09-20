{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Benchmark.Statistics where

import Prologue

import qualified Control.Lens.Aeson                 as Lens
import qualified Data.Set                           as Set
import qualified Data.Yaml                          as Yaml
import qualified Luna.Benchmark.Location            as Location
import qualified Luna.Benchmark.Statistics.Internal as Internal

import Data.Set                (Set)
import Luna.Benchmark.Location (SrcLoc)
import Perf                    (Cycle)



-----------------------
-- === TimeStats === --
-----------------------

-- === Definition === --

data TimeStats = TimeStats
    { _times   :: ![Double] -- in seconds
    , _maxTime :: !Double
    , _minTime :: !Double
    , _avgTime :: !Double
    , _stdTime :: !Double
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''TimeStats


-- === Instances === --

-- Intentional use of division by zero to yield infinity
instance Default TimeStats where
    def = TimeStats def def (1/0) def def

instance Yaml.FromJSON TimeStats where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON TimeStats where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle

instance Semigroup TimeStats where
    (TimeStats t1 max1 min1 _ _) <> (TimeStats t2 max2 min2 _ _)
        = TimeStats times (max max1 max2) (min min1 min2) (Internal.meanF times)
                (Internal.stddevF times) where times = t1 <> t2
    {-# INLINE (<>) #-}

instance StyledShow PrettyShowStyle TimeStats where
    styledShow _ (TimeStats times max min avg stddev)
        =  "Recorded Times: "     <> convert (show times)
        <> "Max Time: "           <> convert (show max)
        <> "Min Time: "           <> convert (show min)
        <> "Mean Time: "          <> convert (show avg)
        <> "Standard Deviation: " <> convert (show stddev)


-----------------------
-- === TickStats === --
-----------------------

-- === Definition === --

data TickStats = TickStats
    { _tickCounts :: ![Cycle] -- In ticks
    , _maxTicks   :: !Cycle
    , _minTicks   :: !Cycle
    , _avgTicks   :: !Cycle
    , _stdTicks   :: !Double
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''TickStats


-- === Instances === --

instance Default TickStats where
    def = TickStats def def (maxBound :: Cycle) def def

instance Yaml.FromJSON TickStats where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON TickStats where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle

instance Semigroup TickStats where
    (TickStats t1 max1 min1 _ _) <> (TickStats t2 max2 min2 _ _)
        = TickStats ticks (max max1 max2) (min min1 min2) (Internal.meanI ticks)
            (Internal.stddevI ticks) where ticks = t1 <> t2
    {-# INLINE (<>) #-}

instance StyledShow PrettyShowStyle TickStats where
    styledShow _ (TickStats times max min avg stddev)
        =  "Recorded Ticks: "     <> convert (show times)
        <> "Max Ticks: "          <> convert (show max)
        <> "Min Ticks: "          <> convert (show min)
        <> "Mean Ticks: "         <> convert (show avg)
        <> "Standard Deviation: " <> convert (show stddev)



-----------------------
-- === MemStats === --
-----------------------

-- === Definition === --

data MemVal a = MemVal
    { _liveBytes   :: !a
    , _actualBytes :: !a
    , _maxBytes    :: !a
    , _numGCs      :: !a
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''MemVal

data MemStats = MemStats
    { _memVals :: ![MemVal Int64]
    , _maxMem  :: !(MemVal Int64)
    , _minMem  :: !(MemVal Int64)
    , _avgMem  :: !(MemVal Int64)
    , _stdMem  :: !(MemVal Double)
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''MemStats


-- === API === --

maximumMemVal :: forall a . (Default a, Ord a) => [MemVal a] -> MemVal a
maximumMemVal []     = def @(MemVal a)
maximumMemVal [x]    = x
maximumMemVal (x:xs) = if x > maximumMemVal xs then x else maximumMemVal xs

minimumMemVal :: forall a . (Default a, Ord a) => [MemVal a] -> MemVal a
minimumMemVal []     = def @(MemVal a)
minimumMemVal [x]    = x
minimumMemVal (x:xs) = if x < minimumMemVal xs then x else minimumMemVal xs

meanMemVal :: [MemVal Int64] -> MemVal Int64
meanMemVal xs = applyListMemVal Internal.meanI $ toListMemVal xs

stdMemVal :: [MemVal Int64] -> MemVal Double
stdMemVal xs = applyListMemVal Internal.stddevI $ toListMemVal xs

toListMemVal :: [MemVal a] -> MemVal [a]
toListMemVal xs = let
    actualBytes = (\(MemVal a _ _ _) -> a) <$> xs
    numGCs      = (\(MemVal _ a _ _) -> a) <$> xs
    liveBytes   = (\(MemVal _ _ a _) -> a) <$> xs
    maxBytes    = (\(MemVal _ _ _ a) -> a) <$> xs
    in MemVal actualBytes numGCs liveBytes maxBytes

applyListMemVal :: ([a] -> b) -> MemVal [a] -> MemVal b
applyListMemVal f (MemVal as bs cs ds)
    = MemVal (f as) (f bs) (f cs) (f ds)


-- === Instances === --

instance (Default a) => Default (MemVal a) where
    def = MemVal def def def def

instance (Yaml.FromJSON a) => Yaml.FromJSON (MemVal a) where
    parseJSON = Lens.parseYamlStyle

instance (Yaml.ToJSON a) => Yaml.ToJSON (MemVal a) where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle

instance Default MemStats where
    def = MemStats def def maxMemVal def def where
        maxVal = maxBound :: Int64
        maxMemVal = MemVal maxVal maxVal maxVal maxVal

instance Yaml.FromJSON MemStats where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON MemStats where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle

instance Semigroup MemStats where
    (MemStats m1 max1 min1 _ _) <> (MemStats m2 max2 min2 _ _)
        = MemStats mem (max max1 max2) (min min1 min2)
            (meanMemVal mem) (stdMemVal mem) where mem = m1 <> m2
    {-# INLINE (<>) #-}

instance StyledShow PrettyShowStyle MemStats where
    styledShow _ (MemStats times max min avg stddev)
        =  "Recorded Allocations: " <> convert (show times)
        <> "Max Bytes: "            <> convert (show max)
        <> "Min Bytes: "            <> convert (show min)
        <> "Mean Bytes: "           <> convert (show avg)
        <> "Standard Deviation: "   <> convert (show stddev)



------------------------
-- === Statistics === --
------------------------

-- === Definition === --

data Statistics = Statistics
    { _locationName    :: !Text
    , _sourceLocations :: !(Set SrcLoc)
    , _timeInfo        :: !TimeStats
    , _tickInfo        :: !TickStats
    , _memInfo         :: !MemStats
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''Statistics


-- === Instances === --

instance Default Statistics where
    def = Statistics def Set.empty def def def

instance Yaml.FromJSON Statistics where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON Statistics where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle

instance Semigroup Statistics where
    (Statistics l1 locs1 time1 tick1 mem1) <> (Statistics _ locs2 time2 tick2 mem2)
        = Statistics l1 (locs1 `Set.union` locs2) (time1 <> time2)
            (tick1 <> tick2) (mem1 <> mem2)
    {-# INLINE (<>) #-}

