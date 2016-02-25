module Data.Build where

import Prologue
import Data.Version.Semantic

data Build = Build { __number  :: Int
                   , __date    :: String -- FIXME[WD]
                   , __hash    :: String
                   , __version :: Version
                   } deriving (Show)
