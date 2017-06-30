{-# EXT InlineAll #-}

module Text.Megaparsec.Ext where

import Prelude

import           Data.List.NonEmpty
import qualified Data.Set             as Set
import           Data.Set             (Set)
import           Data.String
import           Text.Megaparsec
import           Text.Megaparsec.Prim (MonadParsec)


expected :: MonadParsec e s m => ErrorItem (Token s) -> m a
expected = expectedOneOf . Set.singleton

expectedOneOf :: MonadParsec e s m => Set (ErrorItem (Token s)) -> m a
expectedOneOf s = failure Set.empty s Set.empty

unexpectedAnyOf :: MonadParsec e s m => Set (ErrorItem (Token s)) -> m a
unexpectedAnyOf s = failure s Set.empty Set.empty


-- Removing shit from Megaparsec
instance IsString (ErrorItem p) where
    fromString (s:ss) = Label $ s :| ss
