{-
    stack script
    --resolver lts-12.26
    --package base
    --package directory
    --package filepath
    --package lens
    --package optparse-applicative
    --package path
    --package text
    --package turtle
-}

{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE NegativeLiterals       #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}

module Main where

import Prelude hiding (FilePath)

import qualified Data.Either         as Either
import qualified Data.List           as List
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text
import qualified Options.Applicative as Options
import qualified Path                as Path
import qualified System.Directory    as Directory
import qualified System.FilePath     as FilePath
import qualified Turtle              as Turtle
import qualified Turtle.Pattern      as Turtle

import Control.Lens           (makeLenses, (^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad          (when, unless)
import Data.Text              (Text)
import Options.Applicative    ((<**>))
import Path                   (Path, Rel, File, Abs, Dir)
import System.FilePath        (FilePath, (</>))
import Turtle                 ((<|>))



-----------------------
-- === Constants === --
-----------------------

projectYaml :: Path Rel File
projectYaml = $(Path.mkRelFile "./stack.yaml")

releaseOpts :: [String]
releaseOpts = ["-fno-omit-interface-pragmas"]

pkgBaseDirFromLocalBin :: FilePath
pkgBaseDirFromLocalBin = "../../"



-------------------------
-- === CLI Options === --
-------------------------

-- === Definition === --

data CommandOpts = CommandOpts
    { _releaseMode :: Bool
    , _verbose     :: Bool
    , _check       :: Bool
    , _cleanStack  :: Bool
    } deriving (Eq, Show)
makeLenses ''CommandOpts


-- === API === --

optionsParser :: Options.Parser CommandOpts
optionsParser = CommandOpts
    <$> Options.switch (Options.long "release"
        <> Options.help "Build the Luna package in release mode")
    <*> Options.switch (Options.long "verbose"
        <> Options.help "Enable verbose mode while building.")
    <*> Options.switch (Options.long "check"
        <> Options.help "Check that the build script itself builds.")
    <*> Options.switch (Options.long "clean-stack"
        <> Options.help "Clean stack build artefacts before build.")



----------------------
-- === Main API === --
----------------------

runBuilder :: CommandOpts -> Turtle.Shell ()
runBuilder opts = when (not $ opts ^. check) $ do
    liftIO $ print opts

    -- Find the dist folder location
    distributionFolder <- getDistFolder projectYaml

    -- Clean Stack Build State
    when (opts ^. cleanStack) $ do
        cleanResult <- Turtle.inprocWithErr "stack" ["clean"] Turtle.empty
        when (Either.isLeft cleanResult) $ error "Could not clean build."

    -- Clean Package Build State
    pkgRootPath <- getPkgRoot distributionFolder pkgBaseDirFromLocalBin

    pkgRootExists <- liftIO . Directory.doesDirectoryExist
        $ Turtle.encodeString pkgRootPath

    when pkgRootExists $ Turtle.rmtree pkgRootPath

    -- Build and Copy Luna


    -- Copy Licenses and Stdlib

    pure ()

getPkgRoot :: Path Abs Dir -> FilePath -> Turtle.Shell Turtle.FilePath
getPkgRoot binPath relRoot = do
    let fileBinPath = Path.fromAbsDir binPath
        pkgRoot = fileBinPath </> relRoot

    canonicalPkgRoot <- liftIO $ Directory.canonicalizePath pkgRoot

    when (canonicalPkgRoot == "/") $ error "Invalid root \"/\""

    pure $ Turtle.decodeString canonicalPkgRoot

getDistFolder :: Path Rel File -> Turtle.Shell (Path Abs Dir)
getDistFolder inputFile = do
    stackYaml <- liftIO . Text.readFile $ Path.fromRelFile inputFile

    let distLocMatch = Turtle.between
            "local-bin-path: "
            (Turtle.once Turtle.newline)
            (Turtle.star Turtle.anyChar)
        yamlLines = flip Text.append "\n" <$> Text.lines stackYaml
        pathMatch = concat $ Turtle.match distLocMatch <$> yamlLines

    when (length pathMatch /= 1) . error
        $ "Found " <> (show $ length pathMatch) <> " dist folder matches."

    canonicalDistPath <- liftIO . Directory.canonicalizePath . Text.unpack
        $ head pathMatch

    pure =<< Path.parseAbsDir canonicalDistPath

genGHCOptions :: [Text] -> Text
genGHCOptions inputOpts = ghcOptsKey <> quote <> optsFold <> quote
    where ghcOptsKey = "--ghc-options="
          quote = "\""
          optsFold = Text.unwords inputOpts

genBuildOpts :: [Text] -> [Text]
genBuildOpts ghcOpts = ["build", "--copy-bins"] <> genGHCOptions

------------------
-- === Main === --
------------------

main :: IO ()
main = do
    opts <- Options.execParser buildOptsParser
    Turtle.sh $ runBuilder opts
    where buildOptsParser = Options.info (optionsParser <**> Options.helper)
            (Options.fullDesc
                <> Options.progDesc "The package build script for Luna."
                <> Options.header "Visual and textual functional prorgamming.")


-- 1. Use utils to get at dist folder name w/ default (turtle + regexp for
--    practicality)
-- 2. Run build and copy
-- 3. Get data into correct place
-- 4. Check everything is in place for a successful build

