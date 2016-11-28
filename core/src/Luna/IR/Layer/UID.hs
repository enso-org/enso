{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Layer.UID where

import Data.Data (Data)

import Luna.Prelude
import Luna.IR.Layer.Class

import           Control.Monad.State (StateT)
import qualified Control.Monad.State as State



-----------------------
-- === UID Layer === --
-----------------------

newtype ID = ID Word64 deriving (Bounded, Default, Enum, Eq, Integral, Data, Num, Ord, Read, Real, Show)

data UID = UID deriving (Show)

type instance LayerData UID t = ID
