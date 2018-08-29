{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Syntax.Text.IO where

import Prelude

import Conduit
import Control.Monad.Trans.Resource (MonadResource)
import Data.Convert

import           Data.Text32 (Text32)
import qualified Data.Text32 as Text32


-- FIXME[WD]: SourceReader reads bytestring to Data.Text and converts it to Text32 via [Char], which is very inefficient
sourceReader :: (MonadResource m, MonadThrow m) => FilePath -> ConduitM any Text32 m ()
sourceReader t = sourcePreprocessor $ mapOutput convert $ sourceFile t .| decodeUtf8C ; {-# INLINE sourceReader #-}

sourceProducer :: Monad m => Text32 -> ConduitM any Text32 m ()
sourceProducer = yield . tabsToSpaces ; {-# INLINE sourceProducer #-}

-- | Luna does not accept source files with tab characters, so we convert all tabs to spaces before loading the file.
sourcePreprocessor :: Monad m => ConduitM any Text32 m a -> ConduitM any Text32 m a
sourcePreprocessor = mapOutput tabsToSpaces ; {-# INLINE sourcePreprocessor #-}

tabsToSpaces :: Text32 -> Text32
tabsToSpaces = Text32.replace '\t' $ convert (replicate 4 ' ') ; {-# INLINE tabsToSpaces #-}

