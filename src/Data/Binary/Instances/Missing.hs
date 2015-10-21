---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE StandaloneDeriving #-}

module Data.Binary.Instances.Missing where

import           Control.Applicative ((<$>))
import           Data.Binary         (Binary (..))
import           Data.Text.Lazy      (Text)
import qualified Data.Text.Lazy      as Text
import           Prelude
import Data.Version
import GHC.Generics(Generic)



deriving instance Generic Version
instance Binary Version
