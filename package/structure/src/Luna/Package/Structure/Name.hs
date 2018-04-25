module Luna.Package.Structure.Name where

import System.FilePath (FilePath)

-------------------------------------
-- === Package Component Names === --
-------------------------------------

configDir :: FilePath
configDir = ".luna-project"

distDir :: FilePath
distDir = "dist"

srcDir :: FilePath
srcDir = "src"

testDir :: FilePath
testDir = "test"

configFile :: FilePath
configFile = "config.yaml"

depsFile :: FilePath
depsFile = "deps.yaml"

depsHistFile :: FilePath
depsHistFile = "deps-history.yaml"

lirDir :: FilePath
lirDir = ".lir"

mainFile :: FilePath
mainFile = "Main.luna"

licenseFile :: FilePath
licenseFile = "LICENSE"

readmeFile :: FilePath
readmeFile = "README.md"

gitignoreFile :: FilePath
gitignoreFile = ".gitignore"

