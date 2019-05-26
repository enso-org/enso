
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

import Path
import Path.IO
import System.FilePath as FilePath

import qualified Path.Internal as IPath



-- | Extract the last directory name of a path.
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


liftPredicate :: (String -> Bool) -> (Path a b -> Bool)
liftPredicate pred = pred . toFilePath
