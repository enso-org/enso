module Luna.Diagnostic.Info where

import Paths_luna   as PATHS

import Prelude.Luna hiding (Version)

import           Data.Build
import           Data.Version.Semantic
import qualified Data.Version as V


version = Version major minor patch (PreRelease (Tag Alpha Nothing) []) []
    where (major:minor:patch:_) = V.versionBranch (PATHS.version) <> repeat 0

build = Build undefined undefined undefined undefined
