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

import qualified Control.Exception   as Exception
import qualified Data.Either         as Either
import qualified Data.List           as List
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text
import qualified Options.Applicative as Options
import qualified Path                as Path
import qualified System.Directory    as Directory
import qualified System.Environment  as Environment
import qualified System.FilePath     as FilePath
import qualified System.IO.Error     as IOError
import qualified Turtle              as Turtle
import qualified Turtle.Pattern      as Turtle

import Control.Lens           (makeLenses, (^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad          (when, unless)
import Data.Monoid            ((<>))
import Data.Text              (Text)
import Options.Applicative    ((<**>))
import Path                   (Path, Rel, File, Abs, Dir, (</>))
import System.FilePath        (FilePath)
import Turtle                 ((<|>))



-----------------------
-- === Constants === --
-----------------------

defaultDistFolder :: Path Rel Dir
defaultDistFolder = $(Path.mkRelDir "dist/")

-- Relative to package root
relativeBinFolder :: Path Rel Dir
relativeBinFolder = $(Path.mkRelDir "bin/public/")

projectYaml :: Path Rel File
projectYaml = $(Path.mkRelFile "stack.yaml")

releaseOpts :: [Text]
releaseOpts = ["-fno-omit-interface-pragmas"]

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
    , _checkScript :: Bool
    , _cleanStack  :: Bool
    , _packageDir  :: String
    } deriving (Eq, Ord, Show)
makeLenses ''CommandOpts


-- === API === --

optionsParser :: Options.Parser CommandOpts
optionsParser = CommandOpts
    <$> Options.switch (Options.long "release"
        <> (Options.help $ "Build the Luna package in release mode. This will "
            <> "take significantly longer (~1 hour vs. ~5 mins), but will "
            <> "result in significantly increased binary performance."))
    <*> Options.switch (Options.long "verbose"
        <> (Options.help $ "Enable verbose mode while building. This will "
            <> "print all output from stack instead of only printing on a "
            <> "failure."))
    <*> Options.switch (Options.long "check-script"
        <> (Options.help $ "Check that the build script itself builds. This is "
            <> "primarily for usage on CI."))
    <*> Options.switch (Options.long "clean-stack"
        <> (Options.help $ "Clean stack build artefacts before build. This is "
            <> "equivalent to calling `stack clean`."))
    <*> Options.strOption (Options.long "package-dir" <> Options.metavar "DIR"
        <> Options.value (Path.fromRelDir defaultDistFolder)
        <> (Options.help $ "The directory to build the package in. The default "
            <> "value is `" <> Path.fromRelDir defaultDistFolder <> "`."))



----------------------
-- === Main API === --
----------------------

runBuilder :: CommandOpts -> Turtle.Shell ()
runBuilder opts = when (not $ opts ^. checkScript) $ do
    -- Find the dist folder location
    distFP   <- liftIO . Directory.canonicalizePath $ opts ^. packageDir
    distPath <- Path.parseAbsDir distFP
    let binPath = distPath </> relativeBinFolder

    -- Clean Stack Build State (in Subshell)
    when (opts ^. cleanStack) . Turtle.sh $ do
        logMessage "Cleaning stack build artefacts"
        Turtle.void $ Turtle.inprocWithErr "stack" ["clean"] Turtle.empty

    -- Clean Package Build State
    logMessage "Cleaning package dist"
    let packageRootFP = Turtle.decodeString $ Path.fromAbsDir distPath

    liftIO $ Exception.catchJust
        (\e -> if IOError.isDoesNotExistError e then Just () else Nothing)
        (Turtle.rmtree packageRootFP)
        (const $ pure ())

    -- Build and Copy Luna (in Subshell)
    let buildMsg = "Building Luna in " <> if opts ^. releaseMode
            then "release mode"
            else "develop mode"
    logMessage buildMsg

    let ghcOpts   = if opts ^. releaseMode then releaseOpts else []
        buildOpts = genBuildOpts ghcOpts . Text.pack $ Path.fromAbsDir binPath

    -- TODO Want to barf the actual build error if possible.
    liftIO $ Exception.handle
        (\(e :: Turtle.ExitCode) ->
            error "Stack build failed. Please rerun with `--verbose` for info.")
        $ Turtle.sh $ Turtle.inprocWithErr "stack" buildOpts Turtle.empty >>=
            \out -> when (opts ^. verbose) . liftIO $ case out of
                Left err -> Text.putStrLn $ Turtle.lineToText err
                Right ln -> Text.putStrLn $ Turtle.lineToText ln

    -- Create the `config/env` folder
    logMessage "Creating the data folder"
    let dataRootPath = packageRootFP <> dataDistLoc
    Turtle.mktree dataRootPath

    -- Copy Stdlib
    logMessage "Copying the standard library"
    Turtle.cptree stdlibSrcFolderLoc $ dataRootPath <> "stdlib/Std"

    -- Copy Licenses
    logMessage "Copying the licenses"
    Turtle.cptree licenseSrcFolderLoc $ dataRootPath <> "licenses"

    logMessage $ "Package built in " <> show distPath

logMessage :: String -> Turtle.Shell ()
logMessage msg = liftIO . putStrLn $ ">>> " <> msg

genGHCOptions :: [Text] -> Text
genGHCOptions inputOpts = ghcOptsKey <> quote <> optsFold <> quote
    where ghcOptsKey = "--ghc-options="
          quote = "\""
          optsFold = Text.unwords inputOpts

genBuildOpts :: [Text] -> Text -> [Text]
genBuildOpts ghcOpts pkgPath =
    ["--local-bin-path", pkgPath, "install"] <> [genGHCOptions ghcOpts]

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

