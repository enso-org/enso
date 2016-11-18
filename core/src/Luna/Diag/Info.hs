module Luna.Diag.Info where

import Paths_luna as Paths

import Prelude.Luna

import           Data.Build
import           Data.Version.Semantic
import qualified Data.Version as V


version = Version major minor patch (PreRelease (Tag Alpha Nothing) []) []
    where (major:minor:patch:_) = V.versionBranch (Paths.version) <> repeat 0

build = Build $notImplemented $notImplemented $notImplemented $notImplemented
