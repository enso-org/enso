{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Text.Megaparsec.Ext where

import Prelude

import           Data.List.NonEmpty
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.String
import           Text.Megaparsec


expected :: MonadParsec e s m => ErrorItem (Token s) -> m a
expected e = failure (Just e) mempty

instance IsString (ErrorItem p) where
    fromString (s:ss) = Label $ s :| ss

