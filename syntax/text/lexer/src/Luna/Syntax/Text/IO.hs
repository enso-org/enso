{-# LANGUAGE OverloadedStrings #-}

module Luna.Syntax.Text.IO where

import Prelude

import           Conduit
import           Data.Conduit.Combinators     hiding (replicate)
import           Data.Convert
import qualified Data.Text                    as Text
import           Data.Text                    (Text)
import           Control.Monad.Trans.Resource (MonadResource)


sourceReader :: MonadResource m => FilePath -> ConduitM any Text m ()
sourceReader t = sourcePreprocessor $ sourceFile t .| decodeUtf8C ; {-# INLINE sourceReader #-}

sourceProducer :: Monad m => Text -> ConduitM any Text m ()
sourceProducer = yield . tabsToSpaces ; {-# INLINE sourceProducer #-}

-- | Luna does not accept source files with tab characters, so we convert all tabs to spaces before loading the file.
sourcePreprocessor :: Monad m => ConduitM any Text m a -> ConduitM any Text m a
sourcePreprocessor = mapOutput tabsToSpaces ; {-# INLINE sourcePreprocessor #-}

tabsToSpaces :: Text -> Text
tabsToSpaces = Text.replace "\t" . convert $ replicate 4 ' ' ; {-# INLINE tabsToSpaces #-}
