{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Luna.Prim.DynamicLinker (
      loadLibrary
    , loadSymbol
    , closeLibrary
    , Handle
    ) where

import           Prologue hiding (throwM)

import qualified Data.EitherR         as EitherR
import qualified Data.List            as List
import qualified Data.Text            as Text
import qualified Luna.Datafile.Stdlib as Stdlib
import qualified Luna.Package         as Package
import qualified Luna.Path.Path       as Path
import qualified Safe                 as Safe
import qualified Path                 as Path
import qualified Path.IO              as Path
import qualified System.Environment   as Env
import qualified System.FilePath      as FP
import qualified System.Info          as Info (os)
import qualified System.Process       as Process

import Control.Exception.Safe (catchAny, throwM, tryAny)
import Control.Monad.Except   (ExceptT(..), runExceptT)
import Data.Char              (isSpace)
import Data.Maybe             (maybeToList)
import Foreign                (FunPtr)
import System.IO.Unsafe       (unsafePerformIO)
import Path                   (Path, Abs, Rel, Dir, File, (</>))

#if mingw32_HOST_OS
import qualified System.Win32.DLL   as Win32
import qualified System.Win32.Info  as Win32
import qualified System.Win32.Types as Win32 (HINSTANCE)

import Foreign (nullPtr, castPtrToFunPtr)
#else
import qualified System.Posix.DynamicLinker as Unix
#endif



--------------------
-- === Handle === --
--------------------

#if mingw32_HOST_OS
type Handle = Win32.HINSTANCE
#else
type Handle = Unix.DL
#endif



-----------------
-- === API === --
-----------------

nativeLibs :: Path Rel Dir
nativeLibs = $(Path.mkRelDir "native_libs")

data NativeLibraryLoadingException =
    NativeLibraryLoadingException String [String] deriving Show

instance Exception NativeLibraryLoadingException where
    displayException (NativeLibraryLoadingException name details) =
        "Native library " <> name <> " could not be loaded. Details:\n\n"
            <> unlines details

findLocalNativeLibsDirs :: Path Abs Dir -> IO [Path Abs Dir]
findLocalNativeLibsDirs projectDir = do
    let localDepsDir = projectDir </> $(Path.mkRelDir "local_libs")
    localDeps <- tryAny $ Path.listDir localDepsDir
    case localDeps of
        Left  _    -> pure []
        Right (dirs, _) -> do
            localNativeDirs <- for dirs $ \dir ->
                findLocalNativeLibsDirs dir
            pure $ fmap (\a -> a </> nativeLibs) dirs
                  <> concat localNativeDirs

findNativeLibsDirsForProject :: Path Abs Dir -> IO [Path Abs Dir]
findNativeLibsDirsForProject projectDir = do
    let projectNativeDirs =  projectDir </> nativeLibs
    projectLocalDirs      <- findLocalNativeLibsDirs projectDir
    pure $ projectNativeDirs : projectLocalDirs

tryLoad :: Path Abs File -> IO (Either String Handle)
tryLoad path = do
    loadRes <- tryAny $ nativeLoadLibrary path
    let errorDetails exc =
            "loading \"" <> (Path.fromAbsFile path) <> "\" failed with: "
            <> displayException exc
    pure $ EitherR.fmapL errorDetails loadRes

parseError :: [String] -> [String]
parseError e = if ((length $ snd partitioned) == 0) then
        fst partitioned
    else snd partitioned where
    partitioned =
        List.partition (List.isSuffixOf "No such file or directory)") e

loadLibrary :: String -> IO Handle
loadLibrary namePattern = do
    stdlibPath   <- Stdlib.findPath
    projectDir   <- Path.getCurrentDir
    includedLibs <- map snd <$> Package.includedLibs stdlibPath
    nativeDirs   <- fmap concat $
        mapM findNativeLibsDirsForProject (projectDir : includedLibs)

    possibleNames <- mapM Path.parseRelFile
                            [ prefix <> namePattern <> extension
                            | prefix    <- ["lib", ""]
                            , extension <- dynamicLibraryExtensions
                            ]
    let projectNativeDirectories =
            [ nativeDir </> nativeLibraryProjectDir | nativeDir <- nativeDirs ]

    let possiblePaths = [ dir </> name  | dir  <- projectNativeDirectories
                                        , name <- possibleNames
                        ]
    let library = concat $
                  [ "lib" | not ("lib" `List.isPrefixOf` namePattern),
                            Info.os /= "mingw32"
                  ]
               <> [ namePattern ]
               <> [ extension | extension <- dynamicLibraryExtensions,
                                not (null extension)
                  ]
    linkerCache <- maybeToList <$> nativeLoadFromCache library `catchAny`
        const (pure Nothing)
    extendedSearchPaths <- fmap concat . for nativeSearchPaths $ \path -> do
        files <- (Path.listDir path >>= (pure . snd)) `catchAny` \_ -> pure []
        let matchingFiles = filter
                (Path.liftPredicate $ List.isInfixOf library) files
        pure $ matchingFiles
    result <- runExceptT . EitherR.runExceptRT $ do
        let allPaths = possiblePaths <> linkerCache <> extendedSearchPaths
        for allPaths $ \path ->
            EitherR.ExceptRT . ExceptT $ tryLoad path
    case result of
        Left  e -> throwM $ NativeLibraryLoadingException namePattern err where
            err = parseError e
        Right h -> pure h

loadSymbol :: Handle -> String -> IO (FunPtr a)
loadSymbol handle symbol = do
    result <- tryAny $ nativeLoadSymbol handle symbol
    case result of
        Left  e -> throwM $ NativeLibraryLoadingException symbol
                    [displayException e]
        Right h -> pure h

closeLibrary :: Handle -> IO ()
closeLibrary _ = pure ()

#if mingw32_HOST_OS
nativeLoadLibrary :: Path Abs File -> IO Handle
nativeLoadLibrary library = Win32.loadLibraryEx
    (Path.fromAbsFile library) Foreign.nullPtr
    Win32.lOAD_WITH_ALTERED_SEARCH_PATH

nativeLoadSymbol :: Handle -> String -> IO (FunPtr a)
nativeLoadSymbol handle symbol = Foreign.castPtrToFunPtr
    <$> Win32.getProcAddress handle symbol

dynamicLibraryExtensions :: [String]
dynamicLibraryExtensions = ["", ".dll"]

nativeLibraryProjectDir :: Path Rel Dir
nativeLibraryProjectDir = $(Path.mkRelDir "windows")

-- based on https://msdn.microsoft.com/en-us/library/windows/desktop/ms682586(v=vs.85).aspx
nativeSearchPaths :: [Path Abs Dir]
nativeSearchPaths = unsafePerformIO $ do
    exeDirectory <- do
        handle  <- Win32.getModuleHandle Nothing
        exe     <- Win32.getModuleFileName handle >>= Path.parseAbsFile
        pure    $ Path.parent exe
    systemDirectory  <- Win32.getSystemDirectory  >>= Path.parseAbsDir
    windowsDirectory <- Win32.getWindowsDirectory >>= Path.parseAbsDir
    currentDirectory <- Win32.getCurrentDirectory >>= Path.parseAbsDir
    pathDirectories  <- FP.getSearchPath >>= (mapM Path.parseAbsDir)
    pure  $ exeDirectory
          : systemDirectory
          : windowsDirectory
          : currentDirectory
          : pathDirectories

nativeLoadFromCache :: String -> IO (Maybe (Path Abs File))
nativeLoadFromCache _ = return Nothing

#else

-- TODO [JCM] : proposal : an invalid path throws an exception
lookupSearchPath :: String -> IO [Path Abs Dir]
lookupSearchPath env = do
    val <- Env.lookupEnv env
    case val of
        Just envValue   -> mapM Path.parseAbsDir $ FP.splitSearchPath envValue
        Nothing         -> pure []

nativeLoadLibrary :: Path Abs File -> IO Handle
nativeLoadLibrary library = Unix.dlopen
    (Path.fromAbsFile library) [Unix.RTLD_NOW]

nativeLoadSymbol :: Handle -> String -> IO (FunPtr a)
nativeLoadSymbol = Unix.dlsym

dynamicLibraryExtensions :: [String]
nativeLibraryProjectDir :: Path Rel Dir


#if linux_HOST_OS
dynamicLibraryExtensions = [".so", ""]

nativeLibraryProjectDir = $(Path.mkRelDir "linux")

nativeSearchPaths :: [Path Abs Dir]
nativeSearchPaths = unsafePerformIO $ do
    ldLibraryPathDirectories <- lookupSearchPath "LD_LIBRARY_PATH"
    pure $ ldLibraryPathDirectories <>
        [ $(Path.mkAbsDir "/lib")
        , $(Path.mkAbsDir "/usr/lib")
        , $(Path.mkAbsDir "/lib64")
        , $(Path.mkAbsDir "/usr/lib64")
        ]

findBestSOFile :: String -> String -> Maybe (Path Abs File)
findBestSOFile (Text.pack -> namePattern) (Text.pack -> ldconfig) = do
    let matchingLines = filter (Text.isInfixOf namePattern)
                      $ Text.lines ldconfig
        chunks        = map (Text.splitOn " => ") matchingLines
        processSoFile = Text.takeWhile (not . isSpace) . Text.strip
        extractFileAndPath line = case line of
            [soFile, path] -> (processSoFile soFile, path)
            _              -> error "Luna.Prim.DynamicLinker: ldconfig format"
        matchingFiles = map extractFileAndPath chunks
        bestMatches   = List.sortOn (Text.length . fst) matchingFiles
        bestMatch     = Safe.headMay bestMatches
        bestPath      = fmap (Text.unpack . snd) bestMatch
    bestPath >>= Path.parseAbsFile

-- uses /etc/ld.so.cache
nativeLoadFromCache :: String -> IO (Maybe (Path Abs File))
nativeLoadFromCache namePattern = do
    ldconfigCache <- Process.readProcess "ldconfig" ["-p"] ""
    pure $ findBestSOFile namePattern ldconfigCache

#elif darwin_HOST_OS
dynamicLibraryExtensions = [".dylib", ""]

nativeLibraryProjectDir = $(Path.mkRelDir "macos")

-- based on https://developer.apple.com/library/content/documentation/DeveloperTools/Conceptual/DynamicLibraries/100-Articles/UsingDynamicLibraries.html
nativeSearchPaths :: [Path Abs Dir]
nativeSearchPaths = unsafePerformIO $ do
    ldLibraryPathDirectories <- lookupSearchPath "LD_LIBRARY_PATH"
    dyLibraryPathDirectories <- lookupSearchPath "DYLD_LIBRARY_PATH"
    currentDirectory         <- Path.getCurrentDir
    fallbackPathDirectories  <- do
        fallback <- Env.lookupEnv "DYLD_FALLBACK_LIBRARY_PATH"
        case fallback of
            Just val -> mapM Path.parseAbsDir $ FP.splitSearchPath val
            _        -> do
                home <- Env.getEnv "HOME"
                pathPrefix <- Path.parseAbsDir home
                pure $ (pathPrefix </> $(Path.mkRelDir "lib"))
                     : $(Path.mkAbsDir "/usr/local/lib")
                     : $(Path.mkAbsDir "/usr/lib")
                     : []
    pure $ ldLibraryPathDirectories
         <> dyLibraryPathDirectories
         <> [currentDirectory]
         <> fallbackPathDirectories

nativeLoadFromCache :: String -> IO (Maybe (Path Abs File))
nativeLoadFromCache _ = pure Nothing

#endif

#endif
