{-# LANGUAGE OverloadedStrings #-}

module Luna.Syntax.Text.IO where

import Prelude

import           Conduit
import           Data.Conduit.Combinators     hiding (replicate)
import           Data.Convert
import qualified Data.Text                    as Text
import           Data.Text                    (Text)
import           Control.Monad.Trans.Resource (MonadResource)

import           Data.Container.Text32 (Text32)
import qualified Data.Container.Text32 as Text32


sourceReader :: MonadResource m => FilePath -> ConduitM any Text m ()
sourceReader t = sourcePreprocessor $ sourceFile t .| decodeUtf8C ; {-# INLINE sourceReader #-}

sourceReader2 :: MonadResource m => FilePath -> ConduitM any Text32 m ()
sourceReader2 t = mapOutput convert $ sourcePreprocessor $ sourceFile t .| decodeUtf8C ; {-# INLINE sourceReader2 #-}


sourceProducer :: Monad m => Text -> ConduitM any Text m ()
sourceProducer = yield . tabsToSpaces ; {-# INLINE sourceProducer #-}


-- FIXME: so inefficient!
sourceProducer2 :: Monad m => Text32 -> ConduitM any Text32 m ()
sourceProducer2 = yield . tabsToSpaces2 ; {-# INLINE sourceProducer2 #-}
-- sourceProducer2 = mapOutput convert . yield . tabsToSpaces . convert ; {-# INLINE sourceProducer2 #-}

-- | Luna does not accept source files with tab characters, so we convert all tabs to spaces before loading the file.
sourcePreprocessor :: Monad m => ConduitM any Text m a -> ConduitM any Text m a
sourcePreprocessor = mapOutput tabsToSpaces ; {-# INLINE sourcePreprocessor #-}

tabsToSpaces :: Text -> Text
tabsToSpaces = Text.replace "\t" . convert $ replicate 4 ' ' ; {-# INLINE tabsToSpaces #-}

tabsToSpaces2 :: Text32 -> Text32
tabsToSpaces2 = Text32.replace '\t' $ convert (replicate 4 ' ') ; {-# INLINE tabsToSpaces2 #-}
