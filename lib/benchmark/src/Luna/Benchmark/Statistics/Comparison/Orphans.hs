{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Luna.Benchmark.Statistics.Comparison.Orphans where

import Prologue

import Perf (Cycle)



------------------------------
-- === Orphan Instances === --
------------------------------

instance StyledShow Pretty Double where
    styledShow _ a = convert $ show a

instance StyledShow Pretty Int64 where
    styledShow _ a = convert $ show a

instance StyledShow Pretty Cycle where
    styledShow _ a = convert $ show a

