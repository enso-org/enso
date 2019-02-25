module Main where

import Prologue

import qualified Control.Monad.Exception    as Exception
import qualified Control.Monad.Exception.IO as Exception
import qualified Luna.Datafile.Stdlib       as Stdlib
import qualified Luna.Interpreter.Test      as Test
import qualified Luna.Package               as Package
import qualified Luna.Shell.Interpret       as Interpret
import qualified Path                       as Path
import qualified System.Directory           as Directory

import Luna.Datafile        (DatafileException)
import Luna.Shell.Interpret (InterpreterMonad)
import Path                 (Path, Abs, File, Dir, PathException)
import System.FilePath      ((</>))



------------------
-- === Main === --
------------------

main :: IO ()
main = Exception.rethrowFromIO @PathException $ do
    testsDir <- Path.toFilePath <$> Test.directory
    putStrLn $ "Executing benchmarks in " <> testsDir

    benchmarkDirs     <- Directory.listDirectory testsDir
    benchmarkPackages <- sequence $ (Path.parseAbsDir <$> (testsDir </>))
        <$> benchmarkDirs

    for_ benchmarkPackages (\pkgName -> do
        isPkg <- Package.isLunaPackage pkgName
        if isPkg
            then benchmarkPackage (convert $ Path.toFilePath pkgName) pkgName
            else do
                let testFile = pkgName Path.</> Test.standaloneFileName
                isValid <- Directory.doesFileExist $ Path.toFilePath testFile
                if isValid then
                    benchmarkFile (convert $ Path.toFilePath pkgName) testFile
                else putStrLn $ "File " <> Path.toFilePath testFile
                        <> " does not exist.")



-----------------
-- === API === --
-----------------

benchmarkFile :: InterpreterMonad m => Text -> Path Abs File -> m ()
benchmarkFile name file = do
    stdlibLoc <- Exception.catch @DatafileException
        (\e -> error $ "Couldn't find stdlib: " <> displayException e)
        Stdlib.findPath
    putStrLn . convert $ "Benchmarking " <> name
    Interpret.file file stdlibLoc

benchmarkPackage :: InterpreterMonad m => Text -> Path Abs Dir -> m ()
benchmarkPackage name pkg = do
    stdlibLoc <- Exception.catch @DatafileException
        (\e -> error $ "Couldn't find stdlib: " <> displayException e)
        Stdlib.findPath
    putStrLn . convert $ "Benchmarking " <> name
    Interpret.package pkg stdlibLoc

