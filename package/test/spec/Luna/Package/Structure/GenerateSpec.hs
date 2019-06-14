module Luna.Package.Structure.GenerateSpec where

import Prologue

import qualified Luna.Package.Configuration.Global  as Global
import qualified Luna.Package.Configuration.License as License
import qualified Luna.Package.Structure.Name        as Name
import qualified Luna.Path.Path                     as Path
import qualified Path                               as Path
import qualified System.Directory                   as Directory
import qualified System.IO.Temp                     as Temp

import Luna.Package.Structure.Generate ( genPackageStructure
                                       , isValidPkgName )
import System.FilePath                 ( FilePath, (</>) )
import Test.Hspec                      ( Spec, Expectation, it, describe
                                       , shouldBe )



--------------------------------------
-- === Testing Helper Functions === --
--------------------------------------


testPkgDir :: (FilePath -> Expectation) -> Expectation
testPkgDir = Temp.withSystemTempDirectory "pkgTest"

doesExist :: FilePath -> FilePath -> Expectation
doesExist path tempDir = do
    canonicalPath <- Directory.canonicalizePath tempDir
    let packageDir = canonicalPath </> "TestPackage"
    _ <- genPackageStructure
            (Path.unsafeParseAbsDir packageDir) (Just License.MIT)
            $ def @Global.Config

    dirExists <- Directory.doesPathExist (packageDir </> path)

    dirExists `shouldBe` True

findPackageDir :: Bool -> FilePath -> FilePath -> Expectation
findPackageDir shouldFind name rootPath = do
    canonicalPath <- Directory.canonicalizePath rootPath
    result <- genPackageStructure
        (Path.unsafeParseAbsDir (canonicalPath </> name)) (Just License.MIT)
        $ def @Global.Config

    case result of
        Right path -> do
            test <- Directory.doesDirectoryExist (Path.fromAbsDir path)
            test `shouldBe` shouldFind
        Left _ -> shouldFind `shouldBe` False

testNesting :: Bool -> FilePath -> Expectation
testNesting isNested tempPath = do
    let configDir = Path.fromRelDir Name.configDirectory
    canonicalPath <- Directory.canonicalizePath tempPath

    when isNested (Directory.createDirectory $ canonicalPath </> configDir)
    result <- genPackageStructure
        (Path.unsafeParseAbsDir (canonicalPath </> "TestPackage"))
        (Just License.MIT) $ def @Global.Config
    case result of
        Left _ -> isNested `shouldBe` True
        Right _ -> isNested `shouldBe` False



-----------------------
-- === The Tests === --
-----------------------

spec :: Spec
spec = do
    describe "Generates the package directory with the correct name" $ do
        it "Creates the dir if the name is correct" . testPkgDir
            $ findPackageDir True "Foo"
        it "Rejects incorrect names" . testPkgDir $ findPackageDir False "bar"

    describe "Correct generation of top-level package components" $ do
        it "Creates the configuration directory" . testPkgDir
            $ doesExist ".luna-package"
        it "Creates the distribution directory" . testPkgDir
            $ doesExist "dist"
        it "Creates the source directory" . testPkgDir $ doesExist "src"
        it "Creates the license"    . testPkgDir $ doesExist "LICENSE"
        it "Creates the readme"     . testPkgDir $ doesExist "README.md"
        it "Creates the .gitignore" . testPkgDir $ doesExist ".gitignore"

    describe "Correct generation of configuration files" $ do
        it "Creates config.yaml" . testPkgDir
            $ doesExist ".luna-package/config.yaml"
        it "Creates deps.yaml" . testPkgDir
            $ doesExist ".luna-package/deps.yaml"
        it "Creates deps-history.yaml" . testPkgDir
            $ doesExist ".luna-package/deps-history.yaml"
        it "Creates project.lunaproject" . testPkgDir
            $ doesExist ".luna-package/TestPackage.lunaproject"

    describe "Correct generation of stub files" .
        it "Generates the project main" . testPkgDir
            $ doesExist "src/Main.luna"

    describe "Generation of the LIR cache" .
        it "Generates the LIR cache directory" . testPkgDir
            $ doesExist "dist/.lir"

    describe "Package name checking" $ do
        it "Is a valid package name"
            $ isValidPkgName (Path.unsafeParseRelDir "Foo")
        it "Is an invalid packageName" . not
            $ isValidPkgName (Path.unsafeParseRelDir "baAr")

    describe "Detection of nested packages" $ do
        it "Is inside a package" . testPkgDir $ testNesting True
        it "Is not inside a package" . testPkgDir $ testNesting False

