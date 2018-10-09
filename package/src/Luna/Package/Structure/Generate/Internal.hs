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

import Luna.Package.Configuration.License (License)
import System.FilePath                    (FilePath, (</>))



----------------------------------------
-- === Package Generation Helpers === --
----------------------------------------

-- === Definition === --

data GeneratorError
    = InvalidPackageLocation FilePath
    | InvalidPackageName Text
    | SystemError Text
    deriving (Eq, Generic, Ord, Show)


-- === API === --

recovery :: FilePath -> Exception.IOException
         -> IO (Either GeneratorError FilePath)
recovery canonicalName ex = do
    pkgDirExists <- Directory.doesDirectoryExist canonicalName

    let canonicalText :: Text
        canonicalText = convert canonicalName

    if Exception.ioeGetErrorType ex == Exception.AlreadyExists then
        pure . Left . SystemError
            $ "The package at " <> canonicalText <> " already exists."
    else do
        when pkgDirExists $ Directory.removeDirectoryRecursive canonicalName

        let errMsg :: Text
            errMsg = case Exception.ioeGetErrorType ex of
                Exception.NoSuchThing -> "No path to the package directory: "
                    <> canonicalText
                Exception.ResourceBusy ->
                    "Cannot create package on busy media. Please try again."
                Exception.ResourceExhausted -> "Not enough space to create: "
                    <> canonicalText
                Exception.PermissionDenied ->
                    "You do not have permission to create the package: "
                    <> canonicalText
                Exception.HardwareFault -> "Hardware Fault: Please try again."
                Exception.AlreadyExists ->
                    "Some portion of the package already exists: "
                    <> canonicalText
                Exception.InvalidArgument -> "Internal error"
                Exception.InappropriateType ->
                    "Some portion of the package already exists: "
                    <> canonicalText
                Exception.UnsatisfiedConstraints -> "Internal error"
                _ -> convert $ show ex

        pure . Left $ SystemError errMsg


-- === Instances === --

instance StyledShow PrettyShowStyle GeneratorError where
    styledShow _ (InvalidPackageLocation loc) =
        "Cannot create package inside another package at " <> convert loc
    styledShow _ (InvalidPackageName name) = "Invalid name: " <> name
    styledShow _ (SystemError tx) = "System Error: " <> tx



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
        initConfig = (def @Local.Config)
            & Local.license     .~ (fromJust License.None mLicense)
            & Local.projectName .~ (convert pkgName)
            & Local.author      .~ authorName
            & Local.maintainer  .~ maintainer
        pkgName    = unsafeLast $ FilePath.splitDirectories pkgPath
        -- By this point it is guaranteed to have a valid name

    Directory.createDirectory configPath
    IO.appendFile (configPath </> pkgName <> Name.packageExt) ""
    IO.appendFile (configPath </> Name.depsFile) ""
    IO.appendFile (configPath </> Name.depsHistFile) ""

    -- Write the project configuration
    Yaml.encodeFile (configPath </> Name.configFile) initConfig

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

