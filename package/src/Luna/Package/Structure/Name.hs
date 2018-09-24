module Luna.Package.Structure.Name where

import Prologue

import qualified OCI.Data.Name as Name
import qualified Path          as Path

import Path            (Path, Rel, Dir)
import System.FilePath (FilePath)

-------------------------------------
-- === Package Component Names === --
-------------------------------------

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

packageExt :: String
packageExt = ".lunaproject"

lunaRootEnv :: String
lunaRootEnv = "LUNA_LIBS_PATH"

lunaFileExt :: String
lunaFileExt = ".luna"

localLibsPath :: Path Rel Dir
localLibsPath = $(Path.mkRelDir "local_libs")

sourceDirectory :: Path Rel Dir
sourceDirectory = $(Path.mkRelDir "src")

testDirectory :: Path Rel Dir
testDirectory = $(Path.mkRelDir "test")

configDirectory :: Path Rel Dir
configDirectory = $(Path.mkRelDir ".luna-package")

mainFileName :: Name.Qualified
mainFileName = "Main"

mainFuncName :: Name.Qualified
mainFuncName = "main"
