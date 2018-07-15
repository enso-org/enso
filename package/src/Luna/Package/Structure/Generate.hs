module Luna.Package.Structure.Generate
    ( module Luna.Package.Structure.Generate
    , module X ) where

import Luna.Package.Structure.Generate.Internal as X (GeneratorError(..))
import Luna.Package.Structure.Utilities         as X (isValidPkgName)

import Prologue

import qualified Control.Exception                        as Exception
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

genPackageStructure :: MonadIO m => FilePath -> Maybe License
                    -> m (Either GeneratorError FilePath)
genPackageStructure name mLicense = do
    let pkgName = snd $ FilePath.splitFileName name

    canonicalName <- liftIO $ Directory.canonicalizePath name
    insidePkg     <- Utilities.findParentPackageIfInside canonicalName

    let isInsidePkg = isJust insidePkg

    if  | Utilities.isValidPkgName pkgName && not isInsidePkg ->
            liftIO $ Exception.catch create (recovery canonicalName)
        | isInsidePkg -> pure . Left . InvalidPackageLocation
            $ "Cannot create package inside package at: "
            <> (convert $ fromJust "" insidePkg)
        | otherwise -> pure . Left . InvalidPackageName
            $ "Invalid package name: " <> convert canonicalName
    where
        create :: IO (Either GeneratorError FilePath)
        create = do
            canonicalPath <- Directory.canonicalizePath name
            Directory.createDirectoryIfMissing True canonicalPath

            Internal.generateConfigDir       canonicalPath mLicense
            Internal.generateDistributionDir canonicalPath
            Internal.generateSourceDir       canonicalPath
            Internal.generateLicense         canonicalPath mLicense
            Internal.generateReadme          canonicalPath
            Internal.generateGitignore       canonicalPath

            pure $ Right canonicalPath

