module Luna.Package.Configuration.GlobalSpec where

import Prologue

import qualified Luna.Package.Configuration.Global as Global

import Data.ByteString     ( ByteString )
import Luna.YamlUtils.Test ( shouldDecodeAs
                           , shouldNotDecode
                           , shouldGenerate )
import Test.Hspec          ( Spec, describe, it)

allFieldsUsed :: ByteString
allFieldsUsed = [qqStr|
user:
  email: bob@name.com
  name: Bob
license:
  default-license: mit
  generate-license: true
compiler:
  version: '1.3.5'
  options:
  - Foo
  - Bar
  - Baz
|]

allFieldsUsedResult :: Global.Config
allFieldsUsedResult = Global.Config
    (Global.UserConfig "Bob" "bob@name.com")
    (Just (Global.LicenseConfig (Just True) (Just "mit")))
    (Just (Global.CompilerConfig (Just "1.3.5") (Just ["Foo", "Bar", "Baz"])))

missingOptionalFields :: ByteString
missingOptionalFields = [qqStr|
user:
  email: bob@name.com
  name: Bob
|]

missingOptionalFieldsResult :: Global.Config
missingOptionalFieldsResult =
    Global.Config (Global.UserConfig "Bob" "bob@name.com") Nothing Nothing

missingRequiredField :: ByteString
missingRequiredField = [qqStr|
user:
  name  : Bob
|]

invalidKeys :: ByteString
invalidKeys = [qqStr|
user:
  email : bob@name.com
  name  : Bob
  em    : Foo
|]

spec :: Spec
spec = do
    describe "Parses from yaml" $ do
        it "Succeeds with all fields used"
            $ allFieldsUsed `shouldDecodeAs` (Just allFieldsUsedResult)
        it "Succeeds with non-required fields omitted"
            $ missingOptionalFields `shouldDecodeAs`
                (Just missingOptionalFieldsResult)

    describe "Does not parse malformed yaml" $
        it "Fails on missing required keys"
            $ shouldNotDecode @Global.Config missingRequiredField

    describe "Generates correct Yaml from the internal representation" $ do
        it "when optional fields are omitted"
            $ allFieldsUsedResult `shouldGenerate` allFieldsUsed
        it "when all fields are present"
            $ missingOptionalFieldsResult `shouldGenerate` missingOptionalFields

