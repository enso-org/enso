module Luna.Path.Path

where

import Prologue hiding (last)

import qualified Path.Internal    as IPath
import qualified Path             as Path

import qualified System.FilePath  as FilePath

import Data.List            (last)
import Path                 (Path, Rel, Abs, File, Dir)



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

-- | Remove last extension, and the "." preceding it.
dropExtensions :: Path a b -> Path a b
dropExtensions = IPath.Path . FilePath.dropExtensions . Path.toFilePath

-- | Split a path by the directory separator but don't add the trailing
-- slashes to each element.
splitDirectories :: Path a Dir -> [Path Rel Dir]
splitDirectories p =
  map IPath.Path (FilePath.splitDirectories $ Path.toFilePath p)

-- | Is a path a drive.
isDrive :: Path a Dir -> Bool
isDrive = liftPredicate (FilePath.isDrive)

-- | Safely coerce a directory path to a file path.
--
-- The following property holds:
--
-- @coerceToFile $(mkRelDir "a\/b\/c") == $(mkRelFile "a\/b\/c")@
coerceToFile :: Path a Dir -> Path a File
coerceToFile (IPath.Path fp) = IPath.Path fp

-- | Apply the given predicate to the internal ('FilePath')
-- representation of a Path.
liftPredicate :: (String -> Bool) -> (Path a b -> Bool)
liftPredicate pred = pred . Path.toFilePath

-- | Unsafely try to parse a 'FilePath' into a 'Path' 'Rel' 'File'.
unsafeParseRelFile :: FilePath -> Path Rel File
unsafeParseRelFile = unsafeFromJust . Path.parseRelFile

-- | Unsafely try to parse a 'FilePath' into a 'Path' 'Abs' 'File'.
unsafeParseAbsFile :: FilePath -> Path Abs File
unsafeParseAbsFile = unsafeFromJust . Path.parseAbsFile

-- | Unsafely try to parse a 'FilePath' into a 'Path' 'Rel' 'Dir'.
unsafeParseRelDir :: FilePath -> Path Rel Dir
unsafeParseRelDir = unsafeFromJust . Path.parseRelDir

-- | Unsafely try to parse a 'FilePath' into a 'Path' 'Abs' 'Dir'.
unsafeParseAbsDir :: FilePath -> Path Abs Dir
unsafeParseAbsDir = unsafeFromJust . Path.parseAbsDir
