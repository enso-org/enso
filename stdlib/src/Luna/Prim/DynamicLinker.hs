{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Luna.Prim.DynamicLinker (
      loadLibrary
    , loadSymbol
    , closeLibrary
    ) where

import           Luna.Prelude hiding (throwM)

import           Control.Exception.Safe (catchAny, throwM, tryAny)
import           Control.Monad.Except   (ExceptT(..), runExceptT)
import           Data.Char              (isSpace)
import qualified Data.EitherR           as EitherR
import qualified Data.List              as List
import qualified Data.Text              as Text
import           Foreign                (FunPtr)
import qualified Foreign
import qualified Safe
import qualified System.Directory       as Dir
import qualified System.Environment     as Env
import           System.FilePath        ((</>), pathSeparator)
import qualified System.FilePath        as FP
import qualified System.Info            as Info (os)
import qualified System.Process         as Process
import           System.IO.Unsafe       (unsafePerformIO)

#if mingw32_HOST_OS
import qualified System.Win32.DLL   as Win32
import qualified System.Win32.Info  as Win32
import qualified System.Win32.Types as Win32 (HINSTANCE)
#else
import qualified System.Posix.DynamicLinker as Unix
#endif



#if mingw32_HOST_OS
type Handle = Win32.HINSTANCE
#else
type Handle = Unix.DL
#endif

nativeLibs :: FilePath
nativeLibs = "native_libs"

data NativeLibraryLoadingException = NativeLibraryLoadingException String [String]
    deriving Show

instance Exception NativeLibraryLoadingException where
    displayException (NativeLibraryLoadingException name details) =
        "Native library " ++ name ++ " could not be loaded. Details:\n\n" ++ unlines details

findLocalNativeLibsDirs :: FilePath -> IO [FilePath]
findLocalNativeLibsDirs projectDir = do
    let localDepsDir = projectDir </> "local_libs"
    localDeps <- tryAny $ Dir.listDirectory localDepsDir
    case localDeps of
        Left  exc  -> return []
        Right dirs -> do
            localNativeDirs <- forM dirs $ \dir -> do
                findLocalNativeLibsDirs (localDepsDir </> dir)
            return $ map (\a -> localDepsDir </> a </> nativeLibs) dirs
                  <> concat localNativeDirs

tryLoad :: FilePath -> IO (Either String Handle)
tryLoad path = do
    loadRes <- tryAny $ nativeLoadLibrary path
    let errorDetails exc =
            "loading \"" ++ path ++ "\" failed with: " ++ displayException exc
    return $ EitherR.fmapL errorDetails loadRes

loadLibrary :: String -> IO Handle
loadLibrary ""          = cLibrary
loadLibrary namePattern = do
    projectDir <- return ""
    nativeDirs <- (nativeLibs :) <$> findLocalNativeLibsDirs projectDir
    let possibleNames = [ prefix ++ namePattern ++ extension
                        | prefix    <- ["lib", ""]
                        , extension <- dynamicLibraryExtensions
                        ]
        projectNativeDirectories =
            [ nativeDir </> nativeLibraryProjectDir | nativeDir <- nativeDirs ]
        possiblePaths = [ dir </> name | dir  <- ("" : projectNativeDirectories)
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
    linkerCache <- maybeToList <$> nativeLoadFromCache library
    extendedSearchPaths <- fmap concat $ forM nativeSearchPaths $ \path -> do
        files <- Dir.listDirectory path `catchAny` \_ -> return []
        let matchingFiles = filter (List.isInfixOf library) files
        return $ map (path </>) matchingFiles
    result <- runExceptT $ EitherR.runExceptRT $ do
        let allPaths = possiblePaths <> linkerCache <> extendedSearchPaths
        forM allPaths $ \path -> do
            EitherR.ExceptRT $ ExceptT $ tryLoad path
    case result of
        Left  e -> throwM $ NativeLibraryLoadingException namePattern e
        Right h -> return h

loadSymbol :: Handle -> String -> IO (FunPtr a)
loadSymbol handle symbol = nativeLoadSymbol handle symbol

closeLibrary :: Handle -> IO ()
closeLibrary handle = return ()


#if mingw32_HOST_OS
nativeLoadLibrary :: String -> IO Handle
nativeLoadLibrary library = Win32.loadLibrary library

nativeLoadSymbol :: Handle -> String -> IO (FunPtr a)
nativeLoadSymbol handle symbol = Foreign.castPtrToFunPtr <$> Win32.getProcAddress handle symbol

cLibrary :: IO Handle
cLibrary = nativeLoadLibrary "msvcrt"

dynamicLibraryExtensions :: [String]
dynamicLibraryExtensions = [".dll"]

nativeLibraryProjectDir :: String
nativeLibraryProjectDir = "windows"

-- based on https://msdn.microsoft.com/en-us/library/windows/desktop/ms682586(v=vs.85).aspx
nativeSearchPaths :: [FilePath]
nativeSearchPaths = unsafePerformIO $ do
    exeDirectory <- do
        handle <- Win32.getModuleHandle Nothing
        exe    <- Win32.getModuleFileName handle
        return $ FP.takeDirectory exe
    systemDirectory  <- Win32.getSystemDirectory
    windowsDirectory <- Win32.getWindowsDirectory
    currentDirectory <- Win32.getCurrentDirectory
    pathDirectories  <- FP.getSearchPath
    return $ exeDirectory
           : systemDirectory
           : windowsDirectory
           : currentDirectory
           : pathDirectories

nativeLoadFromCache :: String -> IO (Maybe FilePath)
nativeLoadFromCache _ = return Nothing

#else

lookupSearchPath :: String -> IO [FilePath]
lookupSearchPath env = do
    envValue <- fromMaybe "" <$> Env.lookupEnv env
    return $ FP.splitSearchPath envValue

nativeLoadLibrary :: String -> IO Handle
nativeLoadLibrary library = Unix.dlopen library [Unix.RTLD_LAZY]

nativeLoadSymbol :: Handle -> String -> IO (FunPtr a)
nativeLoadSymbol handle symbol = Unix.dlsym handle symbol

cLibrary :: IO Handle
cLibrary = nativeLoadLibrary ""

dynamicLibraryExtensions :: [String]
nativeLibraryProjectDir  :: String
#if linux_HOST_OS
dynamicLibraryExtensions = [".so", ""]

nativeLibraryProjectDir = "linux"

nativeSearchPaths :: [FilePath]
nativeSearchPaths = unsafePerformIO $ do
    ldLibraryPathDirectories <- lookupSearchPath "LD_LIBRARY_PATH"
    return $ ldLibraryPathDirectories
          <> ["/lib", "/usr/lib", "/lib64", "/usr/lib64"]

findBestSOFile :: String -> String -> Maybe FilePath
findBestSOFile (Text.pack -> namePattern) (Text.pack -> ldconfig) =
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
    in bestPath

-- uses /etc/ld.so.cache
nativeLoadFromCache :: String -> IO (Maybe FilePath)
nativeLoadFromCache namePattern = do
    ldconfigCache <- Process.readProcess "ldconfig" ["-p"] ""
    return $ findBestSOFile namePattern ldconfigCache

#elif darwin_HOST_OS
dynamicLibraryExtensions = [".dylib", ""]

nativeLibraryProjectDir = "macos"

-- based on https://developer.apple.com/library/content/documentation/DeveloperTools/Conceptual/DynamicLibraries/100-Articles/UsingDynamicLibraries.html
nativeSearchPaths :: [FilePath]
nativeSearchPaths = unsafePerformIO $ do
    ldLibraryPathDirectories <- lookupSearchPath "LD_LIBRARY_PATH"
    dyLibraryPathDirectories <- lookupSearchPath "DYLD_LIBRARY_PATH"
    currentDirectory         <- Dir.getCurrentDirectory
    fallbackPathDirectories  <- do
        fallback <- Env.lookupEnv "DYLD_FALLBACK_LIBRARY_PATH"
        case fallback of
            Just val -> return $ FP.splitSearchPath val
            _        -> do
                home <- Env.getEnv "HOME"
                return $ (home </> "lib")
                       : "/usr/local/lib"
                       : "/usr/lib"
                       : []
    return $ ldLibraryPathDirectories
          <> dyLibraryPathDirectories
          <> [currentDirectory]
          <> fallbackPathDirectories

nativeLoadFromCache :: String -> IO (Maybe FilePath)
nativeLoadFromCache _ = return Nothing

#endif

#endif