module Luna.PackageSpec where

import Prologue

import qualified Control.Exception                  as Exception
import qualified Data.Yaml                          as Yaml
import qualified Luna.Package                       as Package
import qualified Luna.Package.Configuration.Global  as Global
import qualified Luna.Package.Configuration.License as License
import qualified Luna.Package.Configuration.Local   as Local
import qualified Luna.Package.Structure.Name        as Name
import qualified Path                               as Path
import qualified System.Directory                   as Directory
import qualified System.IO.Temp                     as Temp

import Luna.Package.Structure.Generate ( genPackageStructure )
import Path                            ( Path, Abs, Dir )
import System.FilePath                 ( FilePath, (</>) )
import Test.Hspec                      ( Spec, Expectation, describe, it
                                       , shouldBe, shouldThrow, Selector )



--------------------------------------
-- === Testing Helper Functions === --
--------------------------------------

packageName :: String
packageName = "PackageOne"

packageNewName :: String
packageNewName = "PackageTwo"

packageBadName :: String
packageBadName = "my_bad_package"

shouldFailWithName :: FilePath -> Expectation
shouldFailWithName name = Temp.withSystemTempDirectory "pkgTest" $ \dir ->
    genPackageStructure (dir </> packageName) (Just License.MIT)
        (def @Global.Config) >>= \case
            Left ex    -> Exception.throw ex
            Right path -> do
                origPath <- Path.parseAbsDir path
                newPath  <- Path.parseAbsDir (dir </> name)

                Package.rename origPath newPath `shouldThrow` renameException

hasName :: Local.Config -> Text -> Expectation
hasName cfg name = cfg ^. Local.projectName `shouldBe` name

shouldRenameWith :: FilePath -> Expectation
shouldRenameWith name = Temp.withSystemTempDirectory "pkgTest" $ \dir ->
    genPackageStructure (dir </> packageName) (Just License.MIT)
        (def @Global.Config) >>= \case
            Left ex    -> Exception.throw ex
            Right path -> do
                origPath <- Path.parseAbsDir path
                newPath  <- Path.parseAbsDir (dir </> name)

                renameAndCheck name origPath newPath

renameAndCheck :: FilePath -> Path Abs Dir -> Path Abs Dir -> Expectation
renameAndCheck name origPath newPath = do
    -- Check path moved
    (renamedPath, _) <- Package.rename origPath newPath
    renamedPath `shouldBe` newPath

    -- Check `config.yaml` has new name
    let configPath = Path.fromAbsDir renamedPath
            </> Path.fromRelDir Name.configDirectory
            </> Name.configFile

    Yaml.decodeFileEither configPath >>= \case
        Left _    -> True `shouldBe` False
        Right cfg ->
            cfg `hasName` convert name

    -- Check `*.lunaproject` has been renamed
    let projPath = Path.fromAbsDir renamedPath
            </> Path.fromRelDir Name.configDirectory
            </> name <> Name.packageExt

    projExists <- Directory.doesFileExist projPath
    projExists `shouldBe` True

movesAcrossDevicesTo :: FilePath -> Expectation
movesAcrossDevicesTo newPathPart = Temp.withSystemTempDirectory "test" $ \src ->
    genPackageStructure (src </> packageName) (Just License.MIT) def >>= \case
        Left ex    -> Exception.throw ex
        Right path -> Temp.withTempDirectory newPathPart "test" $ \dest -> do
            let name = "NewName"

            origPath <- Path.parseAbsDir path
            newPath  <- Path.parseAbsDir (dest </> name)

            renameAndCheck name origPath newPath

renameMakesConfigIfMissing :: FilePath -> Expectation
renameMakesConfigIfMissing name = Temp.withSystemTempDirectory "test" $ \src ->
    genPackageStructure (src </> packageName) (Just License.MIT) def >>= \case
        Left ex    -> Exception.throw ex
        Right path -> do
            -- Remove the *.lunaproject file
            let projPath = path </> Path.fromRelDir Name.configDirectory
                    </> Name.configFile

            Directory.removeFile projPath

            -- Create new paths
            origPath <- Path.parseAbsDir path
            newPath  <- Path.parseAbsDir (src </> name)

            renameAndCheck name origPath newPath

renameException :: Selector Package.RenameException
renameException = const True



-----------------------
-- === The Tests === --
-----------------------

spec :: Spec
spec = do
    describe "Renaming of packages" $ do
        it "errors on an invalid name" $ shouldFailWithName packageBadName
        it "successfully renames a package" $ shouldRenameWith packageNewName

    describe "Backwards compatibility when renaming" $
        it "creates config.yaml if missing"
            $ renameMakesConfigIfMissing "PkgTest"

    describe "Renaming across devices" $
        it "moves successfully across filesystems" $ do
            homeDir <- Directory.getHomeDirectory
            movesAcrossDevicesTo homeDir

