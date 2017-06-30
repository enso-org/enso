{-# LANGUAGE UndecidableInstances #-}

module Luna.Session.Info where

import Luna.Prelude
import Paths_luna_core as Paths

import qualified Control.Monad.State   as State
import           Control.Monad.Catch   (MonadMask, MonadCatch, MonadThrow)
import           Data.Build
import           Data.Version.Semantic
import qualified Data.Version as V


-- === Definitions === --

data Info = Info { __version :: Version
                 , __build   :: Build
                 } deriving (Show)
makeLenses ''Info

instance Default Info where
    def = Info v b where
        v = Version major minor patch (PreRelease (Tag Alpha Nothing) []) []
        b = Build $notImplemented $notImplemented $notImplemented $notImplemented
        (major:minor:patch:_) = V.versionBranch (Paths.version) <> repeat 0

instance HasVersion Info where version = info_version
