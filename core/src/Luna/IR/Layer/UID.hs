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

type ID = Word64

data UID = UID deriving (Show)

type instance LayerData UID t = ID
