module Luna.Package.Configuration.LocalSpec where

import Prologue

import qualified Luna.Package.Configuration.License as License
import qualified Luna.Package.Configuration.Local   as Local

import Data.ByteString      ( ByteString )
import Luna.Package.Version ( Version(Version) )
import Luna.YamlUtils.Test  ( shouldDecodeAs
                            , shouldNotDecode
                            , shouldGenerate )
import Test.Hspec           ( Spec, describe, it)

allFieldsUsed :: ByteString
allFieldsUsed = [qqStr|
project-name: Foo
external-targets:
- stage: before-compilation
  tool: gcc
  name: Dep
  output: ../lib
  options:
  - -lfoo
project-version:
  minor: 0
  major: 0
  patch: 1
luna-options:
- Foo
- Bar
- Baz
build-type: library
maintainer: joe@email.com
synopsis: This is a test package.
author: Joe Bloggs
license:
  tag: unknown
  contents: My Custom License
description: This is my package description.
|]

allFieldsUsedResult :: Local.Config
allFieldsUsedResult = Local.Config
    "Joe Bloggs"
    "joe@email.com"
    "Foo"
    (Version 0 0 1 Nothing)
    (License.Unknown "My Custom License")
    Local.Library
    "This is a test package."
    "This is my package description."
    (Just ["Foo", "Bar", "Baz"])
    (Just [Local.Target "Dep" "gcc" "../lib" ["-lfoo"] Local.BeforeCompilation])

optionalFieldsOmitted :: ByteString
optionalFieldsOmitted = [qqStr|
project-name: Foo
project-version:
  minor: 0
  major: 0
  patch: 1
build-type: library
maintainer: joe@email.com
synopsis: This is a test package.
author: Joe Bloggs
license:
  tag: mit
description: This is my package description.
|]

optionalFieldsOmittedResult :: Local.Config
optionalFieldsOmittedResult = Local.Config
    "Joe Bloggs"
    "joe@email.com"
    "Foo"
    (Version 0 0 1 Nothing)
    (License.MIT)
    Local.Library
    "This is a test package."
    "This is my package description."
    Nothing
    Nothing

invalidYaml :: ByteString
invalidYaml = [qqStr|
project-version:
  minor: 0
  major: 0
  patch: 1
|]

spec :: Spec
spec = do
    describe "Parses from Yaml" $ do
        it "Succeeds with all fields used"
            $ allFieldsUsed `shouldDecodeAs` (Just allFieldsUsedResult)
        it "Succeeds with optional fields omitted"
            $ optionalFieldsOmitted `shouldDecodeAs`
                (Just optionalFieldsOmittedResult)

    describe "Does not parse malformed Yaml" $
        it "Fails on missing required keys"
            $ shouldNotDecode @Local.Config invalidYaml

    describe "Generates correct yaml from the internal representation" $ do
        it "when optional fields are omitted"
            $ optionalFieldsOmittedResult `shouldGenerate` optionalFieldsOmitted
        it "when all fields are present"
            $ allFieldsUsedResult `shouldGenerate` allFieldsUsed

