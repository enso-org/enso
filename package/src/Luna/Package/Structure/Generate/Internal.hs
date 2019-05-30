module Luna.Package.Structure.Generate.Internal where

import Prologue

import qualified Control.Monad.Exception                 as MException
import qualified Control.Exception                       as Exception
import qualified Data.Text                               as Text
import qualified Data.Yaml                               as Yaml
import qualified GHC.IO.Exception                        as Exception
import qualified Luna.Package.Configuration.Global       as Global
import qualified Luna.Package.Configuration.License      as License
import qualified Luna.Package.Configuration.License.Data as License
import qualified Luna.Package.Configuration.Local        as Local
import qualified Luna.Package.Structure.Name             as Name
import qualified Luna.Path.Path                          as Path
import qualified Path                                    as Path
import qualified Path.IO                                 as Path
import qualified System.Directory                        as Directory
import qualified System.FilePath                         as FilePath
import qualified System.IO                               as IO
import qualified System.IO.Error                         as Exception

import Luna.Datafile                      (DatafileException)
import Control.Exception                  (IOException)
import Luna.Package.Configuration.License (License)
import System.FilePath                    (FilePath, (</>))
import System.IO                          (hPutStrLn, stderr)



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

recovery :: Path.Path Path.Abs Path.Dir -> Exception.IOException
         -> IO (Either GeneratorError (Path.Path Path.Abs Path.Dir))
recovery canonicalPath ex = do
    pkgDirExists <- Path.doesDirExist canonicalPath

    unless_ (Exception.ioeGetErrorType ex == Exception.AlreadyExists)
        . when pkgDirExists $ Path.removeDirRecur canonicalPath

    pure . Left $ SystemError ex


-- === Instances === --

instance StyledShow PrettyShowStyle GeneratorError where
    styledShow _ ex = convert $ displayException ex



------------------------------------------
-- === Package Component Generation === --
------------------------------------------

-- === API === --


generateConfigDir :: Path.Path Path.Abs Path.Dir -> Maybe License -> Global.Config -> IO ()
generateConfigDir pkgAbsPath mLicense globalCfg = do
    let configPath = pkgAbsPath Path.</> Name.configDirectory
        authorName = globalCfg ^. Global.user . Global.name
        authorMail = globalCfg ^. Global.user . Global.email
        maintainer = if Text.null authorMail then "" else
            authorName <> " <" <> authorMail <> ">"
        pkgPath    = unsafeLast $ Path.splitDirectories pkgAbsPath
        -- By this point it is guaranteed to have a valid name

    Path.createDir configPath

    packageRelFile <- (Path.unsafeParseRelFile (Path.fromRelDir pkgPath)) Path.-<.> Name.packageExt
    let packageAbsFile = configPath Path.</> packageRelFile
    IO.appendFile (Path.fromAbsFile packageAbsFile) ""

    IO.appendFile (Path.fromAbsFile (configPath Path.</> Name.depsFile)) ""
    IO.appendFile (Path.fromAbsFile (configPath Path.</> Name.depsHistFile)) ""

    packageConfig mLicense pkgPath authorName maintainer configPath

packageConfig :: Maybe License -> Path.Path Path.Rel Path.Dir -> Text -> Text -> Path.Path Path.Abs Path.Dir -> IO ()
packageConfig mLicense pkgName authorName maintainer path = do
    let initConfig = (def @Local.Config)
            & Local.license     .~ (fromJust License.None mLicense)
            & Local.projectName .~ (convert $ Path.fromRelDir pkgName)
            & Local.author      .~ authorName
            & Local.maintainer  .~ maintainer

    -- Write the project configuration
    Yaml.encodeFile (Path.fromAbsFile (path Path.</> Name.configFile)) initConfig

generateDistributionDir :: Path.Path Path.Abs Path.Dir -> IO ()
generateDistributionDir pkgPath = do
    let distPath = pkgPath Path.</> Name.distDir

    Path.createDir distPath
    Path.createDir $ distPath Path.</> Name.lirDir

generateSourceDir :: Path.Path Path.Abs Path.Dir -> IO ()
generateSourceDir pkgPath = do
    let srcPath = pkgPath Path.</> Name.srcDir

    liftIO $ Path.createDir srcPath
    IO.appendFile (Path.toFilePath (srcPath Path.</> Name.mainFile)) [qqStr|
import Std.Base

def main:
    None
|]

generateTestDir :: Path.Path Path.Abs Path.Dir -> IO ()
generateTestDir pkgPath = do
    let (testPath ::Path.Path Path.Abs Path.Dir) = pkgPath Path.</> Name.testDir

    liftIO $ Path.createDir testPath
    IO.appendFile (Path.toFilePath (testPath Path.</> Name.mainFile)) [qqStr|
import Std.Base

def main:
    hello = "Hello, world!"
    None
 |]

generateReadme :: Path.Path Path.Abs Path.Dir -> IO ()
generateReadme pkgPath = do
    let pkgName    = unsafeLast $ Path.splitDirectories pkgPath

    IO.appendFile (Path.toFilePath (pkgPath Path.</> Name.readmeFile)) $ "# " <> (Path.toFilePath pkgName)

generateGitignore :: Path.Path Path.Abs Path.Dir -> IO ()
generateGitignore pkgPath = IO.appendFile (Path.toFilePath (pkgPath Path.</> Name.gitignoreFile))
    "# Luna Build Artefacts\n"

generateLicense :: Path.Path Path.Abs Path.Dir -> Maybe License -> IO ()
generateLicense pkgPath mLicense = do
    let licensePath = pkgPath Path.</> Name.licenseFile

    case mLicense of
        Nothing  -> pure ()
        Just key -> case key of
            License.Unknown tx -> IO.appendFile (Path.toFilePath licensePath) $ convert tx
            License.None       -> pure ()
            _                  -> do
                licenseText <- MException.catch @DatafileException
                    (\e -> hPutStrLn stderr (displayException e) >> pure "")
                    $ License.getLicenseText key
                IO.appendFile (Path.toFilePath licensePath) licenseText

