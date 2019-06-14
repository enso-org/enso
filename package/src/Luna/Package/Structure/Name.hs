module Luna.Package.Structure.Name where

import Prologue

import qualified OCI.Data.Name as Name
import qualified Path          as Path

import Path (Path, Rel, Dir, File)



-------------------------------------
-- === Package Component Names === --
-------------------------------------

distDir :: Path Rel Dir
distDir = $(Path.mkRelDir "dist")

srcDir :: Path Rel Dir
srcDir = $(Path.mkRelDir "src")

testDir :: Path Rel Dir
testDir = $(Path.mkRelDir "test")

configFile :: Path Rel File
configFile = $(Path.mkRelFile "config.yaml")

depsFile :: Path Rel File
depsFile = $(Path.mkRelFile "deps.yaml")

depsHistFile :: Path Rel File
depsHistFile = $(Path.mkRelFile "deps-history.yaml")

lirDir :: Path Rel Dir
lirDir = $(Path.mkRelDir ".lir")

mainFile :: Path Rel File
mainFile = $(Path.mkRelFile "Main.luna")

licenseFile :: Path Rel File
licenseFile = $(Path.mkRelFile "LICENSE")

readmeFile :: Path Rel File
readmeFile = $(Path.mkRelFile "README.md")

gitignoreFile :: Path Rel File
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

