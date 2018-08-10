module Luna.Package.VersionSpec where

import Prologue

import qualified Luna.Package.Version as Version

import Luna.Package.Version  (Version(Version), Prerelease(Prerelease))
import Luna.ParserUtils.Test (shouldParseTo, shouldFailToParse)
import Test.Hspec            ( Spec, describe, it, shouldSatisfy
                             , shouldNotSatisfy )

spec :: Spec
spec = do
    describe "Parsing of pre-release types" $ do
        it "literal alpha" $ shouldParseTo "alpha" Version.prereleaseType
                             Version.Alpha
        it "literal beta"  $ shouldParseTo "beta"  Version.prereleaseType
                             Version.Beta
        it "literal rc"    $ shouldParseTo "rc"    Version.prereleaseType
                             Version.RC
        it "invalid type"  $ Version.prereleaseType `shouldFailToParse` "foo"

    describe "Parsing of pre-release tags" $ do
        it "alpha tag"         $ shouldParseTo "alpha.1" Version.prerelease
                                 (Prerelease Version.Alpha 1)
        it "beta tag"          $ shouldParseTo "beta.231" Version.prerelease
                                 (Prerelease Version.Beta 231)
        it "rc tag"            $ shouldParseTo "rc.0" Version.prerelease
                                 (Prerelease Version.RC 0)
        it "silly number"      $ shouldParseTo "alpha.0000" Version.prerelease
                                 (Prerelease Version.Alpha 0)
        it "invalid tag"       $ Version.prerelease `shouldFailToParse` "foo.1"
        it "missing number"    $ Version.prerelease `shouldFailToParse` "alpha."
        it "letter for number" $ Version.prerelease `shouldFailToParse`
                                 "alpha.a"

    describe "Parsing of version strings" $ do
        it "basic version string" $
            shouldParseTo "0.0.1-alpha.0"
            Version.version (Version 0 0 1 (Just (Prerelease Version.Alpha 0)))
        it "omission of patch" $
            shouldParseTo "1.0" Version.version (Version 1 0 0 Nothing)
        it "omission of patch with prerelease" $
            shouldParseTo "12.2-beta.1"
            Version.version (Version 12 2 0 (Just (Prerelease Version.Beta 1)))
        it "omission of minor version" $
            shouldParseTo "12" Version.version (Version 12 0 0 Nothing)
        it "omission of minor version with prerelease" $
            shouldParseTo "12-rc.2" Version.version
            (Version 12 0 0 (Just (Prerelease Version.RC 2)))
        it "omission of prerelease" $
            shouldParseTo "2.3.1" Version.version (Version 2 3 1 Nothing)
        it "invalid version" $ Version.version `shouldFailToParse` "0.0.0"
        it "invalid version with prerelease" $
            Version.version `shouldFailToParse` "0.0.0-alpha.1"

    describe "Version ordering" $ do
        it "correctly ordered versions" $ (Version 0 0 1 Nothing)
            `shouldSatisfy` (< (Version 1 1 2 Nothing))
        it "incorrectly ordered versions" $ (Version 1 1 2 Nothing)
            `shouldNotSatisfy` (< (Version 0 2 1 Nothing))
        it "ordering of prerelease vs no prerelease" $
            (Version 1 2 1 Nothing) `shouldSatisfy`
            (> (Version 1 2 1 (Just (Prerelease Version.Alpha 1))))
        it "ordering within prereleases, same prerelease stage" $
            (Version 1 1 1 (Just (Prerelease Version.Alpha 1))) `shouldSatisfy`
            (< (Version 1 1 1 (Just (Prerelease Version.Alpha 2))))
        it "ordering within prereleases, different prerelease stage" $
            (Version 1 1 1 (Just (Prerelease Version.Alpha 3))) `shouldSatisfy`
            (< (Version 1 1 1 (Just (Prerelease Version.Beta 1))))
        it "equal versions" $ (Version 1 1 1 Nothing) `shouldSatisfy`
            (== (Version 1 1 1 Nothing))
        it "non-equal versions" $ (Version 1 1 1 Nothing) `shouldNotSatisfy`
            (== (Version 1 1 1 (Just (Prerelease Version.Alpha 1))))
        it "random versions without prereleases" $
            (Version 1 7 13 Nothing) `shouldNotSatisfy`
            (> (Version 84 37 52 Nothing))
        it "newer version vs prerelease from older version" $
            (Version 44 31 36 Nothing) `shouldSatisfy`
            (< (Version 71 46 3 (Just (Prerelease Version.Beta 7))))

