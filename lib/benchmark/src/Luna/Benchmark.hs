{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Benchmark where

import Prologue

import qualified Control.Monad.State.Layered          as State
import qualified Criterion.Measurement                as Criterion
import qualified Data.Either                          as Either
import qualified Data.List                            as List
import qualified Data.Map                             as Map
import qualified Data.Set                             as Set
import qualified Data.Time.Clock                      as Clock
import qualified Data.Time.Format                     as Format
import qualified Data.Yaml                            as Yaml
import qualified Luna.Benchmark.State                 as Benchmark
import qualified Luna.Benchmark.Config                as Config
import qualified Luna.Benchmark.Internal              as Internal
import qualified Luna.Benchmark.Output                as Output
import qualified Luna.Benchmark.Location              as Location
import qualified Luna.Benchmark.Statistics            as Statistics
import qualified Luna.Benchmark.Statistics.Comparison as Statistics
import qualified Luna.Benchmark.Statistics.Internal   as Statistics
import qualified Path                                 as Path
import qualified Perf.Cycle                           as Perf
import qualified System.Directory                     as Directory
import qualified Weigh                                as Weigh

import Control.Monad.State.Layered (StateT)
import Luna.Benchmark.Config       (Config)
import Luna.Benchmark.Statistics   (Statistics)
import System.FilePath             ((</>))



--------------------
-- === BenchT === --
--------------------

-- === Definition === --

type BenchT m a = (MonadIO m, NFData a) => StateT Benchmark.State m a

type Bench a = BenchT IO a

type MonadBench m = (Monad m, MonadIO m, State.Monad Benchmark.State m)


-- === API === --

bench :: (NFData a, MonadIO m) => BenchT m a -> m (a, Benchmark.State)
bench = benchWith (def @Config)

benchWith :: forall a m . (NFData a, MonadIO m)
    => Config -> BenchT m a -> m (a, Benchmark.State)
benchWith cfg comp = do
    -- Perform environment setup to maintain accurate measurements
    liftIO . void $ Perf.warmup 100

    -- Evaluate the benchmark
    let config    = if cfg ^. Config.numRuns < 1 then cfg & Config.numRuns .~ 1
                    else cfg
        initState = (def @Benchmark.State) & Benchmark.config .~ config
        outPath   = config ^. Config.outputPath

    ret@(_, state) <- State.runT comp initState

    -- Load in Previous results for comparison
    cwd          <- liftIO $ Directory.getCurrentDirectory
    outDir       <- liftIO . Directory.canonicalizePath
                 $ cwd </> Path.fromRelDir outPath
    outDirExists <- liftIO $ Directory.doesDirectoryExist outDir

    if not outDirExists then do
        compResults <- Statistics.compare (state ^. Benchmark.statsList) []

        Output.displayResults compResults
    else do
        files   <- liftIO $ Directory.listDirectory outDir

        let numHist = config ^. Config.historyCount
            processFiles = drop (length files - numHist) $ List.sort files

        (results :: [Either Yaml.ParseException [Statistics]]) <-
            for processFiles $ \file -> do
                let fPath = outDir </> file
                liftIO $ Yaml.decodeFileEither fPath

        let loadedHist = Either.rights results
            prevResults =
                (Map.fromList . fmap (\s -> (s ^. Statistics.locationName, s)))
                <$> loadedHist

        compResults <- Statistics.compare (state ^. Benchmark.statsList)
            prevResults

        Output.displayResults compResults

    -- Write out results
    time <- liftIO $ Clock.getCurrentTime

    unless outDirExists . liftIO $ Directory.createDirectory outDir

    let fileName = Format.formatTime Format.defaultTimeLocale
            (Format.iso8601DateFormat (Just "-%H-%M-%S")) time
            <> Internal.fileExt

    liftIO . Yaml.encodeFile (outDir </> fileName) . Map.elems
        $ state ^. Benchmark.statsList

    pure ret
{-# NOINLINE benchWith #-}


test :: IO Int
test = fst <$> bench tester where
    tester :: Bench Int
    tester = do
        tick "label" id (1 :: Int)


-----------------
-- === API === --
-----------------

tick :: forall a b m . (NFData a, MonadBench m, Location.HasCallStack)
    => Text -> (b -> a) -> b -> m a
tick label !f !b = do
    numTests <- (^. (Benchmark.config . Config.numRuns))
        <$> State.get @Benchmark.State

    (cycles, result) <- liftIO $ Perf.ticks numTests f b

    -- Update the recorded state
    state <- State.get @Benchmark.State
    let newStat = (def @Statistics)
            & Statistics.locationName .~ label
            & Statistics.sourceLocations %~ Set.insert Location.get
            & Statistics.tickInfo . Statistics.tickCounts .~ cycles
            & Statistics.tickInfo . Statistics.maxTicks   .~ List.maximum cycles
            & Statistics.tickInfo . Statistics.minTicks   .~ List.minimum cycles
            & Statistics.tickInfo . Statistics.avgTicks
                .~ Statistics.meanI cycles
            & Statistics.tickInfo . Statistics.stdTicks
                .~ Statistics.stddevI cycles

    State.put @Benchmark.State $ state & Benchmark.statsList
        %~ Map.insertWith (<>) label newStat

    pure result
{-# NOINLINE tick #-}

time :: forall a b m . (NFData a, MonadBench m, Location.HasCallStack)
    => Text -> (b -> a) -> b -> m a
time label !f !b = do
    numTests <- (^. (Benchmark.config . Config.numRuns))
        <$> State.get @Benchmark.State

    results <- for [1..numTests] $ \_ -> do
        liftIO $ Criterion.initializeTime
        t1 <- liftIO $ Criterion.getTime
        let result = force $ f b
        t2 <- liftIO $ Criterion.getTime
        pure (result, t2 - t1)
    let timeResults = snd <$> results

    state <- State.get @Benchmark.State
    let newStat = (def @Statistics)
            & Statistics.locationName .~ label
            & Statistics.sourceLocations %~ Set.insert Location.get
            & Statistics.timeInfo . Statistics.times .~ timeResults
            & Statistics.timeInfo . Statistics.maxTime
                .~ List.maximum timeResults
            & Statistics.timeInfo . Statistics.minTime
                .~ List.minimum timeResults
            & Statistics.timeInfo . Statistics.avgTime
                .~ Statistics.meanF timeResults
            & Statistics.timeInfo . Statistics.stdTime
                .~ Statistics.stddevF timeResults

    State.put @Benchmark.State $ state & Benchmark.statsList
        %~ Map.insertWith (<>) label newStat

    pure . List.head $ fst <$> results
{-# NOINLINE time #-}

mem :: forall a b m . (NFData a, MonadBench m, Location.HasCallStack)
    => Text -> (b -> a) -> b -> m a
mem label !f !b = do
    numTests <- (^. (Benchmark.config . Config.numRuns))
        <$> State.get @Benchmark.State

    result <- for [1..numTests] $ \_ -> liftIO $ Weigh.weighFuncResult f b
    let memResults = uncurry Statistics.MemVal . snd <$> result

    state <- State.get @Benchmark.State
    let newStat = (def @Statistics)
            & Statistics.locationName .~ label
            & Statistics.sourceLocations %~ Set.insert Location.get
            & Statistics.memInfo . Statistics.memVals .~ memResults
            & Statistics.memInfo . Statistics.maxMem
                .~ Statistics.maximumMemVal memResults
            & Statistics.memInfo . Statistics.minMem
                .~ Statistics.minimumMemVal memResults
            & Statistics.memInfo . Statistics.avgMem
                .~ Statistics.meanMemVal memResults
            & Statistics.memInfo . Statistics.stdMem
                .~ Statistics.stdMemVal memResults

    State.put @Benchmark.State $ state & Benchmark.statsList
        %~ Map.insertWith (<>) label newStat

    pure . List.head $ fst <$> result
{-# NOINLINE mem #-}

measure :: forall a b m . (NFData a, MonadBench m, Location.HasCallStack)
    => Text -> (b -> a) -> b -> m a
measure label !f !b = do
    void $ mem  label f b
    void $ time label f b

    tick label f b
{-# NOINLINE measure #-}

