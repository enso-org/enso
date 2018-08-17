module Luna.PackageSpec where

import Prologue

import qualified Luna.Package                       as Package
import qualified Luna.Package.Configuration.Global  as Global
import qualified Luna.Package.Configuration.License as License
import qualified Luna.Package.Configuration.Local   as Local
import qualified Path                               as Path
import qualified System.Directory                   as Directory
import qualified System.IO.Temp                     as Temp

import Test.Hspec ( Spec, describe, it, shouldSatisfy, shouldNotSatisfy )

spec :: Spec
spec =
    describe "Renaming of packages" $ do
        it "errors on an invalid name" True
        it "successfully renames a package" True

