{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Benchmark.State where

import Prologue

import Data.Map.Strict           (Map)
import Luna.Benchmark.Config     (Config)
import Luna.Benchmark.Statistics (Statistics)



------------------------
-- === State === --
------------------------

-- === Definition === --

type LocationKey = Text

data State = State
    { _currentTestName :: Text
    , _config          :: Config
    , _statsList       :: Map LocationKey Statistics
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''State


-- === Instances === --

instance Default State where
    def = State def def def

