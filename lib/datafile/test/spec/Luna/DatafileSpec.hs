module Luna.DatafileSpec where

import Prologue

import qualified Luna.Datafile          as Datafile
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

locationException :: Selector Datafile.DatafileException
locationException = const True

envVarSetButInvalid :: Selector Datafile.DatafileException
envVarSetButInvalid (Datafile.EnvVarSetButInvalid _ _ _) = True
envVarSetButInvalid _ = False

repositoryHeadNotFoundFrom :: Selector Datafile.DatafileException
repositoryHeadNotFoundFrom (Datafile.RepositoryHeadNotFoundFrom _) = True
repositoryHeadNotFoundFrom _ = False

repositoryHeadFoundButInvalid :: Selector Datafile.DatafileException
repositoryHeadFoundButInvalid (Datafile.RepositoryHeadFoundButInvalid _) = True
repositoryHeadFoundButInvalid _ = False

invalidExecutableDatafile :: Selector Datafile.DatafileException
invalidExecutableDatafile (Datafile.InvalidExecutableLocation _) = True
invalidExecutableDatafile _ = False

pathException :: Selector Path.PathException
pathException = const True


-- === Test Functions === --

data RepoHeadType = RepoExists | RepoNotExists deriving (Eq, Show)

testRepoHeadDiscovery :: RepoHeadType -> Expectation
testRepoHeadDiscovery ty = Temp.withSystemTempDirectory "LunaTemp" $ \fp -> do
    let someDeepPath      = "foo/bar/baz"
        newWorkingDir     = fp </> someDeepPath

    fpPath            <- Path.parseAbsDir fp
    newWorkingDirPath <- Path.parseAbsDir newWorkingDir

    liftIO . Directory.createDirectoryIfMissing True $ fp </> someDeepPath

    Directory.withCurrentDirectory newWorkingDir $ case ty of
        RepoExists -> do
            liftIO . Directory.createDirectoryIfMissing True
                $ fp </> Path.fromRelDir Datafile.gitFolderName

            repoHead <- Datafile.findRepoHead newWorkingDirPath

            repoHead `shouldBe` fpPath

        RepoNotExists -> Datafile.findRepoHead newWorkingDirPath `shouldThrow`
            repositoryHeadNotFoundFrom

data EnvValType = ValidEnvVal | InvalidEnvVal deriving (Eq, Show)

testEnvVal :: EnvValType -> Expectation
testEnvVal ty = Temp.withSystemTempDirectory "LunaTemp" $ \fp -> do
    let envValName         = "SOME_ENV_VAL"
        envValContents     = fp

    envValContentsPath <- Path.parseAbsDir fp
    extraDirPath       <- Path.parseRelDir "Foo"

    let fullPath      = envValContentsPath Path.</> extraDirPath
        verifier path = do
            Directory.doesDirectoryExist $ Path.fromAbsDir
                $ path Path.</> extraDirPath

    case ty of
        ValidEnvVal -> do
            liftIO . Directory.createDirectoryIfMissing True $ Path.fromAbsDir
                fullPath

            result <- Datafile.dataPathFromEnvVar envValName envValContents
                verifier

            result `shouldBe` envValContentsPath

        InvalidEnvVal -> Datafile.dataPathFromEnvVar envValName envValContents
            verifier `shouldThrow` envVarSetButInvalid



-------------------
-- === Tests === --
-------------------

spec :: Spec
spec = do
    describe "Detection of Filesystem root" $ do
        it "Correctly identifies the root"
            $ Datafile.isRoot $(Path.mkAbsDir "/") `shouldBe` True
        it "Correctly identifies a non root directory"
            $ Datafile.isRoot $(Path.mkAbsDir "/home/") `shouldBe` False

    describe "Using the override environment variable" $ do
        it "Returns the value from the variable if valid"
            $ testEnvVal ValidEnvVal
        it "Throws an error if the variable is set to an invalid path"
            $ testEnvVal InvalidEnvVal

    describe "Searching for a repository head" $ do
        it "Finds the repo head if it exist" $ testRepoHeadDiscovery RepoExists
        it "Throws an error when the repo head doesn't exist"
            $ testRepoHeadDiscovery RepoNotExists

