{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Luna.Benchmark.Statistics.Comparison.Orphans where

import Prologue

import Perf (Cycle)



------------------------------
-- === Orphan Instances === --
------------------------------

instance StyledShow PrettyShowStyle Double where
    styledShow _ a = convert $ show a

instance StyledShow PrettyShowStyle Int64 where
    styledShow _ a = convert $ show a

instance StyledShow PrettyShowStyle Cycle where
    styledShow _ a = convert $ show a

