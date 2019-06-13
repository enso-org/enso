module Luna.Path.Path

where

import Prologue hiding (last)

import Control.Monad.Catch (MonadThrow(..))
import Path
import System.FilePath as FilePath
import Data.Maybe (fromJust)
import Data.List (last)

import qualified Path.Internal as IPath


-- | Extract the last directory name of a path, ignoring the ending '/'.
--
-- The following properties hold:
--
-- @dirnameNoSlash $(mkRelDir ".") == $(mkRelDir ".")@
--
-- @dirnameNoSlash (p <\> a) == dirnameNoSlash a@
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

coerceToFile :: Path a Dir -> Path a File
coerceToFile (IPath.Path fp) = IPath.Path fp

liftPredicate :: (String -> Bool) -> (Path a b -> Bool)
liftPredicate pred = pred . toFilePath

unsafeParseRelFile :: FilePath -> Path Rel File
unsafeParseRelFile = unsafeFromJust . parseRelFile

unsafeParseAbsFile :: FilePath -> Path Abs File
unsafeParseAbsFile = unsafeFromJust . parseAbsFile

unsafeParseRelDir :: FilePath -> Path Rel Dir
unsafeParseRelDir = unsafeFromJust . parseRelDir

unsafeParseAbsDir :: FilePath -> Path Abs Dir
unsafeParseAbsDir = unsafeFromJust . parseAbsDir

