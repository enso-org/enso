{-# EXT InlineAll #-}

module Text.Megaparsec.Ext where

import Prelude

import           Data.List.NonEmpty
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.String
import           Text.Megaparsec


expected :: MonadParsec e s m => ErrorItem (Token s) -> m a
expected e = failure (Just e) mempty

-- expectedOneOf :: MonadParsec e s m => Set (ErrorItem (Token s)) -> m a
-- expectedOneOf s = failure Set.empty s Set.empty

-- unexpectedAnyOf :: MonadParsec e s m => Set (ErrorItem (Token s)) -> m a
-- unexpectedAnyOf s = failure s Set.empty Set.empty

instance IsString (ErrorItem p) where
    fromString (s:ss) = Label $ s :| ss
