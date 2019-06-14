module Luna.Package.Structure.Generate
    ( module Luna.Package.Structure.Generate
    , module X ) where

import Luna.Package.Structure.Generate.Internal as X (GeneratorError(..))
import Luna.Package.Structure.Utilities         as X (isValidPkgName)

import Prologue

import qualified Control.Exception                        as Exception
import qualified Luna.Package.Configuration.Global        as Global
import qualified Luna.Package.Structure.Generate.Internal as Internal
import qualified Luna.Package.Structure.Utilities         as Utilities
import qualified Luna.Path.Path                           as Path
import qualified Path                                     as Path
import qualified Path.IO                                  as Path

import Path      (Path, Abs, Dir)

import Luna.Package.Configuration.License       (License)
import Luna.Package.Structure.Generate.Internal (recovery)



--------------------------------
-- === Project Generation === --
--------------------------------

-- === API === --

genPackageStructure :: MonadIO m
    => Path a Dir -> Maybe License -> Global.Config
    -> m (Either GeneratorError (Path Abs Dir))
genPackageStructure path mLicense gblConf =
    if Path.liftPredicate (\name -> length name < 1) path then
        pure . Left . InvalidPackageName $ convert (Path.toFilePath path)
    else do
        -- This is safe as it has at least one component if `path` is nonemtpy
        -- `path` is nonempty due to the guard above.
        let pkgName = unsafeLast $ Path.splitDirectories path

        canonicalPath <- Path.canonicalizePath path
        insidePkg     <- Utilities.findParentPackageIfInside canonicalPath

        case (Utilities.isValidPkgName pkgName , insidePkg) of
            (True, Nothing) -> liftIO
                $ Exception.catch create (recovery canonicalPath)
            (_, Just pkg) -> pure . Left . InvalidPackageLocation $ pkg
            _ ->  pure . Left . InvalidPackageName
                $ convert (Path.fromAbsDir canonicalPath)
        where
            create :: IO (Either GeneratorError (Path Abs Dir))
            create = do
                canonicalPath <- Path.canonicalizePath path
                Path.createDirIfMissing True canonicalPath

                Internal.generateConfigDir       canonicalPath mLicense gblConf
                Internal.generateDistributionDir canonicalPath
                Internal.generateSourceDir       canonicalPath
                Internal.generateLicense         canonicalPath mLicense
                Internal.generateReadme          canonicalPath
                Internal.generateGitignore       canonicalPath

                pure $ Right canonicalPath
