module Luna.Test.Build.Dependency.VersionSpec where

import Luna.Build.Dependency.Version

import Prologue

import Luna.Test.Build.Dependency.ParserTestUtils

import Test.Hspec

spec :: Spec
spec = do
    describe "Parsing of pre-release types" $ do
        it "literal alpha" $ shouldParseTo "alpha" prereleaseType Alpha
        it "literal beta"  $ shouldParseTo "beta"  prereleaseType Beta
        it "literal rc"    $ shouldParseTo "rc"    prereleaseType RC
        it "invalid type"  $ prereleaseType `shouldFailToParse` "foo"

    describe "Parsing of pre-release tags" $ do
        it "alpha tag"         $ shouldParseTo "alpha.1" prerelease
                                 (Prerelease Alpha 1)
        it "beta tag"          $ shouldParseTo "beta.231" prerelease
                                 (Prerelease Beta 231)
        it "rc tag"            $ shouldParseTo "rc.0" prerelease
                                 (Prerelease RC 0)
        it "silly number"      $ shouldParseTo "alpha.0000" prerelease
                                 (Prerelease Alpha 0)
        it "invalid tag"       $ prerelease `shouldFailToParse` "foo.1"
        it "missing number"    $ prerelease `shouldFailToParse` "alpha."
        it "letter for number" $ prerelease `shouldFailToParse` "alpha.a"

    describe "Parsing of version strings" $ do
        it "basic version string" $
            shouldParseTo "0.0.1-alpha.0"
            version (Version 0 0 1 (Just (Prerelease Alpha 0)))
        it "omission of patch" $
            shouldParseTo "1.0" version (Version 1 0 0 Nothing)
        it "omission of patch with prerelease" $
            shouldParseTo "12.2-beta.1"
            version (Version 12 2 0 (Just (Prerelease Beta 1)))
        it "omission of minor version" $
            shouldParseTo "12" version (Version 12 0 0 Nothing)
        it "omission of minor version with prerelease" $
            shouldParseTo "12-rc.2" version
            (Version 12 0 0 (Just (Prerelease RC 2)))
        it "omission of prerelease" $
            shouldParseTo "2.3.1" version (Version 2 3 1 Nothing)
        it "invalid version" $ version `shouldFailToParse` "0.0.0"
        it "invalid version with prerelease" $
            version `shouldFailToParse` "0.0.0-alpha.1"

    describe "Version ordering" $ do
        it "correctly ordered versions" $ (Version 0 0 1 Nothing)
            `shouldSatisfy` (< (Version 1 1 2 Nothing))
        it "incorrectly ordered versions" $ (Version 1 1 2 Nothing)
            `shouldNotSatisfy` (< (Version 0 2 1 Nothing))
        it "ordering of prerelease vs no prerelease" $
            (Version 1 2 1 Nothing) `shouldSatisfy`
            (> (Version 1 2 1 (Just (Prerelease Alpha 1))))
        it "ordering within prereleases, same prerelease stage" $
            (Version 1 1 1 (Just (Prerelease Alpha 1))) `shouldSatisfy`
            (< (Version 1 1 1 (Just (Prerelease Alpha 2))))
        it "ordering within prereleases, different prerelease stage" $
            (Version 1 1 1 (Just (Prerelease Alpha 3))) `shouldSatisfy`
            (< (Version 1 1 1 (Just (Prerelease Beta 1))))
        it "equal versions" $ (Version 1 1 1 Nothing) `shouldSatisfy`
            (== (Version 1 1 1 Nothing))
        it "non-equal versions" $ (Version 1 1 1 Nothing) `shouldNotSatisfy`
            (== (Version 1 1 1 (Just (Prerelease Alpha 1))))
        it "random versions without prereleases" $
            (Version 1 7 13 Nothing) `shouldNotSatisfy`
            (> (Version 84 37 52 Nothing))
        it "newer version vs prerelease from older version" $
            (Version 44 31 36 Nothing) `shouldSatisfy`
            (< (Version 71 46 3 (Just (Prerelease Beta 7))))

    describe "Conversion between Version tyoes" $ do
        it "version with no prerelease" $ versionToSolverVersion
            (Version 1 1 2 Nothing) `shouldBe` (SolverVersion 1 1 2 3 0)
        it "version with prerelease" $ versionToSolverVersion
            (Version 11 31 2 (Just (Prerelease Beta 5))) `shouldBe`
            (SolverVersion 11 31 2 1 5)
        it "solverVersion with no prerelease" $ solverVersionToVersion
            (SolverVersion 1 42 5 3 5) `shouldBe` (Version 1 42 5 Nothing)
        it "solverVersion with prerelease" $ solverVersionToVersion
            (SolverVersion 32 1 0 0 2) `shouldBe`
            (Version 32 1 0 (Just (Prerelease Alpha 2)))

