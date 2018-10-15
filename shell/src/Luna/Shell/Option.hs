module Luna.Shell.Option (module Luna.Shell.Option, module X) where

import Options.Applicative as X ( execParser, info, fullDesc, progDesc, header
                                , helper )

import Prologue hiding (init)

import qualified Luna.Shell.Command   as Command
import qualified Options.Applicative  as Options

import Luna.Shell.Command   (Command, CommandOpts)
import Options.Applicative  (Parser)



----------------------------------
-- === Command-Line Parsers === --
----------------------------------

-- === API === --

topLevel :: Parser CommandOpts
topLevel = version <|> lunaCommand

version :: Parser CommandOpts
version = Options.flag' Command.ShowVersion (Options.long "version"
    <> Options.help "Print the current version of Luna.")

lunaCommand :: Parser CommandOpts
lunaCommand = Command.Exec <$> Options.hsubparser
    (  Options.command "run" (Options.info run
        (Options.progDesc "Execute a luna package, or standalone file."))
    <> Options.command "init" (Options.info init
        (Options.progDesc "Initialise a new luna package."))
    <> Options.command "document" (Options.info document
        (Options.progDesc "Print documentation in JSON format."))
    <> Options.command "rename" (Options.info rename
        (Options.progDesc "Rename a package.")))

run :: Parser Command
run = Command.Run <$> (Command.RunOpts
    <$> Options.strOption (Options.long "target"
        <> Options.metavar "FILE/FOLDER" <> Options.value ""
        <> Options.help "Execute FILE/FOLDER in interpreted mode."))

document :: Parser Command
document = Command.Document <$> (Command.DocumentOpts
    <$> Options.strOption (Options.long "target"
        <> Options.metavar "PROJECT" <> Options.value ""
        <> Options.help "Create documentation for PROJECT.")
    <*> Options.strOption (Options.long "out"
        <> Options.metavar "FILE" <> Options.value ""
        <> Options.help "Specify the output file"))

init :: Parser Command
init = Command.Init <$> (Command.InitOpts
    <$> Options.argument Options.str (Options.metavar "PACKAGE-NAME")
    <*> Options.strOption (Options.long "luna-version"
        <> Options.metavar "VERSION" <> Options.value ""
        <> Options.help
            "Initialise the package with the provided Luna version.")
    <*> Options.strOption (Options.long "license"
        <> Options.metavar "LICENSE" <> Options.value ""
        <> Options.help "Initialise the package with the specified license."))

rename :: Parser Command
rename = Command.Rename <$> ( Command.RenameOpts
    <$> Options.argument Options.str (Options.metavar "SOURCE-NAME")
    <*> Options.argument Options.str (Options.metavar "DEST-NAME"))

build :: Parser Command
build = Command.Build <$> (Command.BuildOpts
    <$> Options.switch (Options.long "acquire-deps"
        <> Options.help "Acquire package dependencies but do not build.")
    <*> Options.switch (Options.long "clean-build"
        <> Options.help "Clean all build artefacts before building.")
    <*> Options.strOption (Options.long "file"
        <> Options.metavar "FILE" <> Options.value ""
        <> Options.help "Build a standalone FILE."))

test :: Parser Command
test = Command.Test <$> (Command.TestOpts
    <$> Options.switch (Options.long "no-build"
        <> Options.help "Do not rebuild tests before executing.")
    <*> Options.switch (Options.long "no-bench"
        <> Options.help "Do not run benchmarking tests."))

clean :: Parser Command
clean = Command.Clean <$> (Command.CleanOpts
    <$> Options.switch (Options.long "all"
        <> Options.help "Clean all build artefacts.")
    <*> Options.switch (Options.long "docs"
        <> Options.help "Clean generated documentation.")
    <*> Options.switch (Options.long "cache"
        <> Options.help "Clean the package build cache, including LIR."))

publish :: Parser Command
publish = Command.Publish <$> (Command.PublishOpts
    <$> Options.switch (Options.long "major"
        <> Options.help "Increase the major version of the package.")
    <*> Options.switch (Options.long "minor"
        <> Options.help "Increase the minor version of the package.")
    <*> Options.switch (Options.long "patch"
        <> Options.help "Increase the patch version of the package.")
    <*> Options.strOption (Options.long "prerelease"
        <> Options.metavar "PRERELEASE" <> Options.value ""
        <> Options.help "Increase the prerelease version to PRERELEASE."))

retract :: Parser Command
retract = Command.Retract <$> (Command.RetractOpts
    <$> Options.argument Options.str (Options.metavar "VERSION"))

options :: Parser Command
options = Command.Options <$> (Command.OptionOpts
    <$> Options.many (Options.argument Options.str (Options.metavar "OPTION")))

rollback :: Parser Command
rollback = Command.Rollback <$> (Command.RollbackOpts
    <$> Options.argument Options.str (Options.value ""
        <> Options.metavar "HASH"))

update :: Parser Command
update = Command.Update <$> (Command.UpdateOpts
    <$> Options.some (Options.argument Options.str (Options.value ""
        <> Options.metavar "DEPENDENCY")))

freeze :: Parser Command
freeze = Command.Freeze <$> (Command.FreezeOpts
    <$> Options.some (Options.argument Options.str
        (Options.metavar "DEPENDENCIES")))

unfreeze :: Parser Command
unfreeze = Command.Unfreeze <$> (Command.FreezeOpts
    <$> Options.some (Options.argument Options.str
        (Options.metavar "DEPENDENCIES")))

install :: Parser Command
install = Command.Install <$> (Command.InstallOpts
    <$> Options.some (Options.argument Options.str
        (Options.metavar "PACKAGES")))

download :: Parser Command
download = Command.Download <$> (Command.DownloadOpts
    <$> Options.some (Options.argument Options.str
        (Options.metavar "PACKAGES")))

