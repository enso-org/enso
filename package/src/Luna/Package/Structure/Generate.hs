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
import qualified System.Directory                         as Directory
import qualified System.FilePath                          as FilePath

import Luna.Package.Configuration.License       (License)
import Luna.Package.Structure.Generate.Internal (recovery)
import System.FilePath                          (FilePath)

--------------------------------
-- === Project Generation === --
--------------------------------

-- === API === --

genPackageStructure :: MonadIO m => FilePath -> Maybe License -> Global.Config
                    -> m (Either GeneratorError FilePath)
genPackageStructure name mLicense gblConf =
    if length name < 1 then
        pure . Left . InvalidPackageName $ "Invalid package name: "
            <> convert name
    else do
        -- This is safe as it has at least one component if `name` is nonemtpy
        -- `name` is nonempty due to the guard above.
        let pkgName = unsafeLast $ FilePath.splitDirectories name

        canonicalName <- liftIO $ Directory.canonicalizePath name
        insidePkg     <- Utilities.findParentPackageIfInside canonicalName

        let isInsidePkg = isJust insidePkg

        if  | Utilities.isValidPkgName pkgName && not isInsidePkg ->
                liftIO $ Exception.catch create (recovery canonicalName)
            | isInsidePkg -> pure . Left . InvalidPackageLocation
                $ fromJust "" insidePkg
            | otherwise -> pure . Left . InvalidPackageName
                $ convert canonicalName
        where
            create :: IO (Either GeneratorError FilePath)
            create = do
                canonicalPath <- Directory.canonicalizePath name
                Directory.createDirectoryIfMissing True canonicalPath

                Internal.generateConfigDir       canonicalPath mLicense gblConf
                Internal.generateDistributionDir canonicalPath
                Internal.generateSourceDir       canonicalPath
                Internal.generateLicense         canonicalPath mLicense
                Internal.generateReadme          canonicalPath
                Internal.generateGitignore       canonicalPath

                pure $ Right canonicalPath

