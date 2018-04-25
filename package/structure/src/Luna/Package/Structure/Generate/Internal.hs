module Luna.Package.Structure.Generate.Internal where

import Prologue

import qualified Control.Exception             as Exception
import qualified GHC.IO.Exception              as Exception
import qualified Luna.Package.Structure.Name   as Name
import qualified System.Directory              as Directory
import qualified System.IO                     as IO
import qualified System.IO.Error               as Exception

import System.FilePath (FilePath, (</>))

----------------------------------------
-- === Package Generation Helpers === --
----------------------------------------

-- === Definition === --

data GeneratorError
    = InvalidPackageLocation Text
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



------------------------------------------
-- === Package Component Generation === --
------------------------------------------

-- === API === --

generateConfigDir :: FilePath -> IO ()
generateConfigDir pkgPath = do
    let configPath = pkgPath </> Name.configDir

    -- TODO [Ara] Populate these files
    Directory.createDirectory configPath
    IO.appendFile (configPath </> Name.configFile) ""
    IO.appendFile (configPath </> Name.depsFile) ""
    IO.appendFile (configPath </> Name.depsHistFile) ""

generateDistributionDir :: FilePath -> IO ()
generateDistributionDir pkgPath = do
    let distPath = pkgPath </> Name.distDir

    Directory.createDirectory distPath
    Directory.createDirectory $ distPath </> Name.lirDir

-- TODO [Ara] Populate the main files.
generateSourceDir :: FilePath -> IO ()
generateSourceDir pkgPath = do
    let srcPath = pkgPath </> Name.srcDir

    Directory.createDirectory srcPath
    IO.appendFile (srcPath </> Name.mainFile) ""

generateTestDir :: FilePath -> IO ()
generateTestDir pkgPath = do
    let testPath = pkgPath </> Name.testDir

    Directory.createDirectory testPath
    IO.appendFile (testPath </> Name.mainFile) ""

-- TODO [Ara] Populate these files
generateLicense :: FilePath -> IO ()
generateLicense pkgPath = IO.appendFile (pkgPath </> Name.licenseFile) ""

generateReadme :: FilePath -> IO ()
generateReadme pkgPath = IO.appendFile (pkgPath </> Name.readmeFile) ""

generateGitignore :: FilePath -> IO ()
generateGitignore pkgPath = IO.appendFile (pkgPath </> Name.gitignoreFile) ""

