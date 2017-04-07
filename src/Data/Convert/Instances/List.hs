module Data.Convert.Instances.List where

import Prelude
import Data.Convert.Class
import Data.List.NonEmpty



type instance ConversionError [a] (NonEmpty a) = SimpleConversionError
instance PartialConvertible   [a] (NonEmpty a) where
    tryConvert = \case
        []     -> Left simpleConversionError
        (e:es) -> Right $ e :| es
