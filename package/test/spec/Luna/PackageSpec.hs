module Luna.PackageSpec where

import Prologue

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
            Left _     -> True `shouldBe` False
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
            Left _ -> True `shouldBe` False
            Right path -> do
                origPath <- Path.parseAbsDir path
                newPath  <- Path.parseAbsDir (dir </> name)

                -- Check path moved
                renamedPath <- Package.rename origPath newPath
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

renameException :: Selector Package.RenameException
renameException = const True



-----------------------
-- === The Tests === --
-----------------------

spec :: Spec
spec =
    describe "Renaming of packages" $ do
        it "errors on an invalid name" $ shouldFailWithName packageBadName
        it "successfully renames a package" $ shouldRenameWith packageNewName

