{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Benchmark.Config where

import Prologue

import qualified Path as Path

import Path (Path, Rel, Dir)



--------------------
-- === Config === --
--------------------

-- === Definition === --

data Config = Config
    { _numRuns          :: !Int            -- Num of runs to average
    , _outputPath       :: !(Path Rel Dir) -- Output directory
    , _historyCount     :: !Int            -- Num history items to compare with
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''Config


-- === Instances === --

instance Default Config where
    def = Config 5 $(Path.mkRelDir "./bench-results") 1

