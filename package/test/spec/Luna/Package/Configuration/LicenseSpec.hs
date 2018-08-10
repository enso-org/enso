module Luna.Package.Configuration.LicenseSpec where

import Prologue

import Luna.Package.Configuration.License as License

import Data.ByteString                    ( ByteString )
import Luna.Package.Configuration.License ( License )
import Luna.ParserUtils.Test              ( shouldParseTo
                                          , shouldFailToParse )
import Luna.YamlUtils.Test                ( shouldDecodeAs
                                          , shouldGenerate )
import Test.Hspec                         ( Spec, describe, it )

knownLicense :: ByteString
knownLicense = [qqStr|
tag: mit
|]

knownLicenseResult :: License
knownLicenseResult = License.MIT

unknownLicense :: ByteString
unknownLicense = [qqStr|
tag: unknown
contents: my-custom-license
|]

unknownLicenseResult :: License
unknownLicenseResult = License.Unknown "my-custom-license"

spec :: Spec
spec = do
    describe "Parsing of license literals" $ do
        it "Parses 'none'" $ shouldParseTo "none" License.license License.None
        it "Parses 'mit'"  $ shouldParseTo "mit"  License.license License.MIT
        it "Parses unknown Licenses" $ shouldParseTo "my-license"
            License.license (License.Unknown "my-license")
        it "Empty license" $ shouldFailToParse  License.license ""

    describe "Generation of Yaml" $ do
        it "Generates valid yaml in the known license case"
            $ knownLicense `shouldDecodeAs` (Just knownLicenseResult)
        it "Generates valid yaml in the unknown license case"
            $ unknownLicense `shouldDecodeAs` (Just unknownLicenseResult)

    describe "Decoding of Yaml" $ do
        it "Generates valid representation in the known license case"
            $ knownLicenseResult `shouldGenerate` knownLicense
        it "Generates valid representation in the unknown license case"
            $ unknownLicenseResult `shouldGenerate` unknownLicense

