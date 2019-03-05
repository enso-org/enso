{-
    stack script
    --resolver lts-12.26
    --package base
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

import qualified Data.Text           as Text
import qualified Options.Applicative as Options
import qualified Path                as Path
import qualified Turtle              as Turtle

import Control.Lens           (makeLenses, (^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text              (Text)
import Options.Applicative    ((<**>))
import Path                   (Path, Rel, File, Abs)



-----------------------
-- === Constants === --
-----------------------

projectYaml :: Path Rel File
projectYaml = $(Path.mkRelFile "./stack.yaml")

releaseOpts :: [String]
releaseOpts = ["-fno-omit-interface-pragmas"]



----------------------
-- === Main API === --
----------------------

-- TODO use Turtle.inProcWithErr
runBuilder :: CommandOpts -> Turtle.Shell ()
runBuilder opts = do
    liftIO $ print opts
    pure ()

getDistFolder :: Path Rel File -> Path Abs File
getDistFolder = undefined

-- genGHCOptions :: Text


-------------------------
-- === CLI Options === --
-------------------------

data CommandOpts = CommandOpts
    { _releaseMode :: Bool
    , _verbose     :: Bool
    } deriving (Eq, Show)
makeLenses ''CommandOpts

optionsParser :: Options.Parser CommandOpts
optionsParser = CommandOpts
    <$> Options.switch (Options.long "release"
        <> Options.help "Build the Luna package in release mode")
    <*> Options.switch (Options.long "verbose"
        <> Options.help "Enable verbose mode while building.")



------------------
-- === Main === --
------------------

main :: IO ()
main = do
    opts <- Options.execParser buildOptsParser where
    buildOptsParser = Options.info (optionsParser <**> Options.helper)
        (Options.fullDesc
            <> Options.progDesc "The package build script for Luna."
            <> Options.header "Visual and textual functional prorgamming.")


-- 1. Use utils to get at dist folder name w/ default (turtle + regexp for
--    practicality)
-- 2. Run build and copy
-- 3. Get data into correct place
-- 4. Check everything is in place for a successful build

