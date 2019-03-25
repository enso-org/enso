{-
    stack script
    --resolver lts-11.22
    --package base
    --package directory
    --package filepath
    --package lens
    --package optparse-applicative
    --package path
    --package text
    --package turtle
-}

-- | Luna Packaging Script
--
-- This script exists to create distributable packages for Luna, including the
-- datafiles that a `luna` executable needs to function. It generates a package
-- as follows in the package root:
--
-- dist/
--  |- bin/public/luna
--  |- config/env/
--      |- stdlib/
--      |- licenses/
--
-- This script must be run from its containing directory.
--
-- TODO Make it robust against when running from other directories.

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
import qualified System.Environment  as Environment
import qualified System.FilePath     as FilePath
import qualified Turtle              as Turtle
import qualified Turtle.Pattern      as Turtle

import Control.Lens           (makeLenses, (^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad          (when, unless)
import Data.Monoid            ((<>))
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

releaseOpts :: [Text]
releaseOpts = ["-fno-omit-interface-pragmas"]

pkgBaseDirFromLocalBin :: FilePath
pkgBaseDirFromLocalBin = "../../"

stdlibSrcFolderLoc :: Turtle.FilePath
stdlibSrcFolderLoc = "stdlib/Std/"

licenseSrcFolderLoc :: Turtle.FilePath
licenseSrcFolderLoc = "package/data/licenses"

dataDistLoc :: Turtle.FilePath
dataDistLoc = "config/env/"



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
    -- Find the dist folder location
    distributionFolder <- getDistFolder projectYaml

    -- Clean Stack Build State (in Subshell)
    when (opts ^. cleanStack) . Turtle.sh $ do
        logMessage "Cleaning stack build artefacts"
        Turtle.void $ Turtle.inprocWithErr "stack" ["clean"] Turtle.empty

    -- Clean Package Build State
    logMessage "Cleaning package dist"
    pkgRootPath <- getPkgRoot distributionFolder pkgBaseDirFromLocalBin

    pkgRootExists <- liftIO . Directory.doesDirectoryExist
        $ Turtle.encodeString pkgRootPath

    when pkgRootExists $ Turtle.rmtree pkgRootPath

    -- Build and Copy Luna (in Subshell)
    let buildMsg = "Building Luna in " <> if opts ^. releaseMode
            then "release mode"
            else "develop mode"
    logMessage buildMsg

    let ghcOpts   = if opts ^. releaseMode then releaseOpts else []
        buildOpts = genBuildOpts ghcOpts

    Turtle.sh $ Turtle.inprocWithErr "stack" buildOpts Turtle.empty >>= \out ->
        when (opts ^. verbose) . liftIO $ case out of
            Left err -> Text.putStrLn $ Turtle.lineToText err
            Right ln -> Text.putStrLn $ Turtle.lineToText ln

    -- Create the `config/env` folder
    logMessage "Creating the data folder"
    let dataRootPath = pkgRootPath <> dataDistLoc
    Turtle.mktree dataRootPath

    -- Copy Stdlib
    logMessage "Copying the standard library"
    Turtle.cptree stdlibSrcFolderLoc $ dataRootPath <> "stdlib/Std"

    -- Copy Licenses
    logMessage "Copying the licenses"
    Turtle.cptree licenseSrcFolderLoc $ dataRootPath <> "licenses"

    logMessage "Package built"

logMessage :: String -> Turtle.Shell ()
logMessage msg = liftIO . putStrLn $ ">>> " <> msg

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
genBuildOpts ghcOpts = ["build", "--copy-bins"] <> [genGHCOptions ghcOpts]

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

