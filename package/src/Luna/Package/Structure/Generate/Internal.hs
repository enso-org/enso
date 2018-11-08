module Luna.Package.Structure.Generate.Internal where

import Prologue

import qualified Control.Exception                       as Exception
import qualified Data.Text                               as Text
import qualified Data.Yaml                               as Yaml
import qualified GHC.IO.Exception                        as Exception
import qualified Luna.Package.Configuration.Global       as Global
import qualified Luna.Package.Configuration.License      as License
import qualified Luna.Package.Configuration.License.Data as License
import qualified Luna.Package.Configuration.Local        as Local
import qualified Luna.Package.Structure.Name             as Name
import qualified Path                                    as Path
import qualified System.Directory                        as Directory
import qualified System.FilePath                         as FilePath
import qualified System.IO                               as IO
import qualified System.IO.Error                         as Exception

import Control.Exception                  (IOException)
import Luna.Package.Configuration.License (License)
import System.FilePath                    (FilePath, (</>))



----------------------------------------
-- === Package Generation Helpers === --
----------------------------------------

-- === Definition === --

data GeneratorError
    = InvalidPackageLocation FilePath
    | InvalidPackageName Text
    | SystemError IOException
    deriving (Eq, Generic, Show)

instance Exception GeneratorError where
    displayException (InvalidPackageLocation fp) = fp
        <> " is an invalid location for a package."
    displayException (InvalidPackageName name) = convert name
        <> " is not a valid package name."
    displayException (SystemError ex) = "System Error: " <> displayException ex


-- === API === --

recovery :: FilePath -> Exception.IOException
         -> IO (Either GeneratorError FilePath)
recovery canonicalName ex = do
    pkgDirExists <- Directory.doesDirectoryExist canonicalName

    unless_ (Exception.ioeGetErrorType ex == Exception.AlreadyExists)
        . when pkgDirExists $ Directory.removeDirectoryRecursive canonicalName

    pure . Left $ SystemError ex


-- === Instances === --

instance StyledShow PrettyShowStyle GeneratorError where
    styledShow _ ex = convert $ displayException ex



------------------------------------------
-- === Package Component Generation === --
------------------------------------------

-- === API === --

generateConfigDir :: FilePath -> Maybe License -> Global.Config -> IO ()
generateConfigDir pkgPath mLicense globalCfg = do
    let configPath = pkgPath </> Path.fromRelDir Name.configDirectory
        authorName = globalCfg ^. Global.user . Global.name
        authorMail = globalCfg ^. Global.user . Global.email
        maintainer = if Text.null authorMail then "" else
            authorName <> " <" <> authorMail <> ">"
        pkgName    = unsafeLast $ FilePath.splitDirectories pkgPath
        -- By this point it is guaranteed to have a valid name

    Directory.createDirectory configPath
    IO.appendFile (configPath </> pkgName <> Name.packageExt) ""
    IO.appendFile (configPath </> Name.depsFile) ""
    IO.appendFile (configPath </> Name.depsHistFile) ""

    packageConfig mLicense pkgName authorName maintainer configPath

packageConfig :: Maybe License -> FilePath -> Text -> Text -> FilePath -> IO ()
packageConfig mLicense pkgName authorName maintainer path = do
    let initConfig = (def @Local.Config)
            & Local.license     .~ (fromJust License.None mLicense)
            & Local.projectName .~ (convert pkgName)
            & Local.author      .~ authorName
            & Local.maintainer  .~ maintainer

    -- Write the project configuration
    Yaml.encodeFile (path </> Name.configFile) initConfig

generateDistributionDir :: FilePath -> IO ()
generateDistributionDir pkgPath = do
    let distPath = pkgPath </> Name.distDir

    Directory.createDirectory distPath
    Directory.createDirectory $ distPath </> Name.lirDir

generateSourceDir :: FilePath -> IO ()
generateSourceDir pkgPath = do
    let srcPath = pkgPath </> Name.srcDir

    Directory.createDirectory srcPath
    IO.appendFile (srcPath </> Name.mainFile) [qqStr|
import Std.Base

def main:
    None
|]

generateTestDir :: FilePath -> IO ()
generateTestDir pkgPath = do
    let testPath = pkgPath </> Name.testDir

    Directory.createDirectory testPath
    IO.appendFile (testPath </> Name.mainFile) [qqStr|
import Std.Base

def main:
    hello = "Hello, world!"
    None
|]

generateReadme :: FilePath -> IO ()
generateReadme pkgPath = do
    let pkgName    = unsafeLast $ FilePath.splitDirectories pkgPath

    IO.appendFile (pkgPath </> Name.readmeFile) $ "# " <> pkgName

generateGitignore :: FilePath -> IO ()
generateGitignore pkgPath = IO.appendFile (pkgPath </> Name.gitignoreFile)
    "# Luna Build Artefacts\n"

generateLicense :: FilePath -> Maybe License -> IO ()
generateLicense pkgPath mLicense = do
    let licensePath = pkgPath </> Name.licenseFile

    case mLicense of
        Nothing  -> pure ()
        Just key -> case key of
            License.Unknown tx -> IO.appendFile licensePath $ convert tx
            License.None       -> pure ()
            _                  -> do
                licenseText <- License.getLicenseText key
                IO.appendFile licensePath licenseText

