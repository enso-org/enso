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
import qualified System.IO                               as IO
import qualified System.IO.Error                         as Exception

import Luna.Datafile                      (DatafileException)
import Control.Exception                  (IOException)
import Luna.Package.Configuration.License (License)
import System.IO                          (hPutStrLn, stderr)

import Path (Path, Abs, Rel, Dir, (</>), (-<.>), fromAbsFile, fromRelDir)



----------------------------------------
-- === Package Generation Helpers === --
----------------------------------------

-- === Definition === --

data GeneratorError
    = InvalidPackageLocation (Path Abs Dir)
    | InvalidPackageName Text
    | SystemError IOException
    deriving (Eq, Generic, Show)

instance Exception GeneratorError where
    displayException (InvalidPackageLocation fp) = Path.fromAbsDir fp
        <> " is an invalid location for a package."
    displayException (InvalidPackageName name) = convert name
        <> " is not a valid package name."
    displayException (SystemError ex) = "System Error: " <> displayException ex


-- === API === --

recovery :: Path Abs Dir -> Exception.IOException
         -> IO (Either GeneratorError (Path Abs Dir))
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

generateConfigDir :: Path Abs Dir -> Maybe License -> Global.Config -> IO ()
generateConfigDir pkgAbsPath mLicense globalCfg = do
    let configPath = pkgAbsPath </> Name.configDirectory
        authorName = globalCfg ^. Global.user . Global.name
        authorMail = globalCfg ^. Global.user . Global.email
        maintainer = if Text.null authorMail then "" else
            authorName <> " <" <> authorMail <> ">"
        pkgPath    = unsafeLast $ Path.splitDirectories pkgAbsPath
        -- By this point it is guaranteed to have a valid name

    Path.createDir configPath

    packageRelFile <- (Path.coerceToFile pkgPath) -<.> Name.packageExt
    let packageAbsFile = configPath </> packageRelFile
    IO.appendFile (fromAbsFile packageAbsFile) ""

    IO.appendFile (fromAbsFile (configPath </> Name.depsFile)) ""
    IO.appendFile (fromAbsFile (configPath </> Name.depsHistFile)) ""

    packageConfig mLicense pkgPath authorName maintainer configPath

packageConfig :: Maybe License -> Path Rel Dir -> Text -> Text -> Path Abs Dir
    -> IO ()
packageConfig mLicense pkgName authorName maintainer path = do
    let initConfig = (def @Local.Config)
            & Local.license     .~ (fromJust License.None mLicense)
            & Local.projectName .~ (convert $ fromRelDir pkgName)
            & Local.author      .~ authorName
            & Local.maintainer  .~ maintainer

    -- Write the project configuration
    Yaml.encodeFile (fromAbsFile (path </> Name.configFile)) initConfig

generateDistributionDir :: Path Abs Dir -> IO ()
generateDistributionDir pkgPath = do
    let distPath = pkgPath </> Name.distDir

    Path.createDir distPath
    Path.createDir $ distPath </> Name.lirDir

generateSourceDir :: Path Abs Dir -> IO ()
generateSourceDir pkgPath = do
    let srcPath = pkgPath </> Name.srcDir

    liftIO $ Path.createDir srcPath
    IO.appendFile (fromAbsFile (srcPath </> Name.mainFile)) [qqStr|
import Std.Base

def main:
    None
|]

generateTestDir :: Path Abs Dir -> IO ()
generateTestDir pkgPath = do
    let testPath = pkgPath </> Name.testDir

    liftIO $ Path.createDir testPath
    IO.appendFile (fromAbsFile (testPath </> Name.mainFile)) [qqStr|
import Std.Base

def main:
    hello = "Hello, world!"
    None
|]

generateReadme :: Path Abs Dir -> IO ()
generateReadme pkgPath = do
    let pkgName    = unsafeLast $ Path.splitDirectories pkgPath

    IO.appendFile (fromAbsFile (pkgPath </> Name.readmeFile))
        $ "# " <> (fromRelDir pkgName)

generateGitignore :: Path Abs Dir -> IO ()
generateGitignore pkgPath = IO.appendFile
    (fromAbsFile (pkgPath </> Name.gitignoreFile)) "# Luna Build Artefacts\n"

generateLicense :: Path Abs Dir -> Maybe License -> IO ()
generateLicense pkgPath mLicense = do
    let licensePath = pkgPath </> Name.licenseFile

    case mLicense of
        Nothing  -> pure ()
        Just key -> case key of
            License.Unknown tx -> IO.appendFile (fromAbsFile licensePath)
                $ convert tx
            License.None       -> pure ()
            _                  -> do
                licenseText <- MException.catch @DatafileException
                    (\e -> hPutStrLn stderr (displayException e) >> pure "")
                    $ License.getLicenseText key
                IO.appendFile (fromAbsFile licensePath) licenseText

