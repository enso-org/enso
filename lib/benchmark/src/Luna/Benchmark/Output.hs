{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Benchmark.Output where

import Prologue

import Luna.Benchmark.Statistics.Comparison (ComparisonResult)



-----------------
-- === API === --
-----------------

displayResults :: MonadIO m => [ComparisonResult] -> m ()
displayResults results = for_ results $ \result ->
    putStrLn . convert $ prettyShow result <> "\n"

