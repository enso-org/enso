
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Path.Path

where

import Control.Monad.Catch (MonadThrow(..))
import Path
import System.FilePath as FilePath
import Data.Maybe (fromJust)

import qualified Path.Internal as IPath


-- | Extract the last directory name of a path, ignoring the ending '/'.
--
-- The following properties hold:
--
-- @dirname $(mkRelDir ".") == $(mkRelDir ".")@
--
-- @dirname (p <\> a) == dirname a@
--
dirnameNoSlash :: Path b Dir -> Path Rel Dir
dirnameNoSlash (IPath.Path "") = IPath.Path ""
dirnameNoSlash (IPath.Path l) | FilePath.isDrive l = IPath.Path ""
dirnameNoSlash (IPath.Path l) = IPath.Path (last (FilePath.splitDirectories l))


dropExtensions :: Path a b -> Path a b
dropExtensions = IPath.Path . FilePath.dropExtensions . toFilePath

splitDirectories :: Path a Dir -> [Path Rel Dir]
splitDirectories p = map IPath.Path (FilePath.splitDirectories $ toFilePath p)

isDrive :: Path a Dir -> Bool
isDrive = liftPredicate (FilePath.isDrive)

liftPredicate :: (String -> Bool) -> (Path a b -> Bool)
liftPredicate pred = pred . toFilePath

unsafeParseRelFile :: FilePath -> Path Rel File
unsafeParseRelFile = fromJust . parseRelFile

unsafeParseAbsFile :: FilePath -> Path Abs File
unsafeParseAbsFile = fromJust . parseAbsFile

unsafeParseRelDir :: FilePath -> Path Rel Dir
unsafeParseRelDir = fromJust . parseRelDir

unsafeParseAbsDir :: FilePath -> Path Abs Dir
unsafeParseAbsDir = fromJust . parseAbsDir

-- TODO JCM : use Convertible class here?
relDirToFile ::  (MonadThrow m) => Path Rel Dir -> m (Path Rel File)
relDirToFile dir = parseRelFile (fromRelDir dir)

absDirToFile ::  (MonadThrow m) => Path Abs Dir -> m (Path Abs File)
absDirToFile dir = parseAbsFile (fromAbsDir dir)
