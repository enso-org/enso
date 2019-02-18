module Luna.Datafile.LocationSpec where

import Prologue

import qualified Luna.Datafile.Location as Location
import qualified Path                   as Path
import qualified System.Directory       as Directory
import qualified System.IO.Temp         as Temp

import System.FilePath ((</>))
import Test.Hspec      ( Spec, Expectation, describe, it, shouldBe, shouldThrow
                       , Selector )



-----------------------------
-- === Testing Support === --
-----------------------------

-- === Exception Selectors === --

locationException :: Selector Location.LocationException
locationException = const True

envVarSetButInvalid :: Selector Location.LocationException
envVarSetButInvalid (Location.EnvVarSetButInvalid _ _) = True
envVarSetButInvalid _ = False

repositoryHeadNotFoundFrom :: Selector Location.LocationException
repositoryHeadNotFoundFrom (Location.RepositoryHeadNotFoundFrom _) = True
repositoryHeadNotFoundFrom _ = False

repositoryHeadFoundButInvalid :: Selector Location.LocationException
repositoryHeadFoundButInvalid (Location.RepositoryHeadFoundButInvalid _) = True
repositoryHeadFoundButInvalid _ = False

pathException :: Selector Path.PathException
pathException = const True


-- === Test Functions === --

data RepoHeadType = RepoExists | RepoNotExists deriving (Eq, Show)

checkRepoHead :: RepoHeadType -> Expectation
checkRepoHead ty = Temp.withSystemTempDirectory "LunaTemp" $ \fp -> do
    let dummyDeepDir = "foo/bar/baz"

    pathTmpDir   <- Path.parseAbsDir fp
    pathDummyDir <- Path.parseRelDir dummyDeepDir

    liftIO . Directory.createDirectoryIfMissing True $ fp </> dummyDeepDir

    case ty of
        RepoExists -> do
            liftIO . Directory.createDirectory
                $ fp </> Path.fromRelDir Location.gitFolderName

            liftIO . Directory.withCurrentDirectory (fp </> dummyDeepDir) $ do
                foundRepoHead <- Location.findRepoHead
                    (pathTmpDir Path.</> pathDummyDir)

                foundRepoHead `shouldBe` pathTmpDir
        RepoNotExists -> liftIO . Directory.withCurrentDirectory
            (fp </> dummyDeepDir)
            $ Location.findRepoHead (pathTmpDir Path.</> pathDummyDir)
                `shouldThrow` repositoryHeadNotFoundFrom

data DefaultPathType
    = DefaultPathExists
    | LocalRepoPath
    | NoRepoPath
    deriving (Eq, Show)

testDefaultPath :: DefaultPathType -> Expectation
testDefaultPath ty = Temp.withSystemTempDirectory "LunaTemp" $ \fp -> do
    fpPath <- Path.parseAbsDir fp

    let dummyDeepDir   = "foo/bar/baz"
        stdlibPath     = fpPath Path.</> Location.stdlibRelPath
        stdlibFullPath = stdlibPath Path.</> Location.stdlibContents
        workingPath    = case ty of
            DefaultPathExists -> fp
            _                 -> fp </> dummyDeepDir

    liftIO . Directory.createDirectoryIfMissing True $ fp </> dummyDeepDir

    liftIO . Directory.withCurrentDirectory workingPath $ case ty of
        DefaultPathExists -> do
            liftIO . Directory.createDirectoryIfMissing True
                $  Path.fromAbsDir stdlibFullPath

            resultPath <- Location.getStdlibDefaultPath

            resultPath `shouldBe` stdlibPath
        LocalRepoPath -> do
            liftIO . Directory.createDirectory
                $ fp </> Path.fromRelDir Location.gitFolderName

            liftIO . Directory.createDirectoryIfMissing True
                $ Path.fromAbsDir stdlibFullPath

            resultPath <- Location.getStdlibDefaultPath

            resultPath `shouldBe` stdlibPath
        NoRepoPath -> do
            liftIO . Directory.createDirectory
                $ fp </> Path.fromRelDir Location.gitFolderName

            Location.getStdlibDefaultPath `shouldThrow`
                repositoryHeadFoundButInvalid

data EnvValType = ValidEnvVal | InvalidEnvVal deriving (Eq, Show)

testEnvVal :: EnvValType -> Expectation
testEnvVal ty = Temp.withSystemTempDirectory "LunaTemp" $ \fp -> do
    fpPath <- Path.parseAbsDir fp

    let stdlibPath = fpPath Path.</> Location.stdlibRelPath
        stdlibFullPath = stdlibPath Path.</> Location.stdlibContents

    case ty of
        ValidEnvVal -> do
            liftIO . Directory.createDirectoryIfMissing True
                $ Path.fromAbsDir stdlibFullPath

            envResult <- Location.getPathOverrideFromEnvVar
                $ Path.fromAbsDir stdlibPath

            envResult `shouldBe` stdlibPath

        InvalidEnvVal -> do
            Location.getPathOverrideFromEnvVar (Path.fromAbsDir stdlibPath)
                `shouldThrow` envVarSetButInvalid



-------------------
-- === Tests === --
-------------------

spec :: Spec
spec = do
    describe "Detection of Filesystem root" $ do
        it "Correctly identifies the root"
            $ Location.isRoot $(Path.mkAbsDir "/") `shouldBe` True
        it "Correctly identifies a non root directory"
            $ Location.isRoot $(Path.mkAbsDir "/home/") `shouldBe` False

    describe "Detection of the repository head" $ do
        it "Finds the repo when it exists" $ checkRepoHead RepoExists
        it "Throws an error when the repo head is not found"
            $ checkRepoHead RepoNotExists

    describe "Getting the default path" $ do
        it "Returns the local path if it exists"
            $ testDefaultPath DefaultPathExists
        it "Returns a path in the repo if found" $ testDefaultPath LocalRepoPath
        it "Throws an error when it can't find the stdlib in the repo"
            $ testDefaultPath NoRepoPath

    describe "Using the override environment variable" $ do
        it "Returns the value from the variable if valid"
            $ testEnvVal ValidEnvVal
        it "Throws an error if the variable is set to an invalid path"
            $ testEnvVal InvalidEnvVal

