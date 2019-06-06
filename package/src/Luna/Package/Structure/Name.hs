module Luna.Package.Structure.Name where

import Prologue

import qualified OCI.Data.Name as Name
import qualified Path          as Path

import Path            (Path, Rel, Dir)
import System.FilePath (FilePath)

-------------------------------------
-- === Package Component Names === --
-------------------------------------

distDir :: Path.Path Path.Rel Path.Dir
distDir = $(Path.mkRelDir "dist")

srcDir :: Path.Path Path.Rel Path.Dir
srcDir = $(Path.mkRelDir "src")

testDir :: Path.Path Path.Rel Path.Dir
testDir = $(Path.mkRelDir "test")

configFile :: Path.Path Path.Rel Path.File
configFile = $(Path.mkRelFile "config.yaml")

depsFile :: Path.Path Path.Rel Path.File
depsFile = $(Path.mkRelFile "deps.yaml")

depsHistFile :: Path.Path Path.Rel Path.File
depsHistFile = $(Path.mkRelFile "deps-history.yaml")

lirDir :: Path.Path Path.Rel Path.Dir
lirDir = $(Path.mkRelDir ".lir")

mainFile :: Path.Path Path.Rel Path.File
mainFile = $(Path.mkRelFile "Main.luna")

licenseFile :: Path.Path Path.Rel Path.File
licenseFile = $(Path.mkRelFile "LICENSE")

readmeFile :: Path.Path Path.Rel Path.File
readmeFile = $(Path.mkRelFile "README.md")

gitignoreFile :: Path.Path Path.Rel Path.File
gitignoreFile = $(Path.mkRelFile ".gitignore")

packageExt :: String
packageExt = "lunaproject"

packageExtWithDot :: String
packageExtWithDot = ".lunaproject"

lunaFileExt :: String
lunaFileExt = "luna"

lunaFileExtWithDot :: String
lunaFileExtWithDot = ".luna"

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

