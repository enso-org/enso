{-# LANGUAGE CPP #-}

module Luna.Prim.DynamicLinker where

import           Luna.Prelude hiding (throwM)

import           Control.Exception.Safe (throwM, tryAny)
import           Control.Monad.Except   (ExceptT(..), runExceptT)
import qualified Data.EitherR           as EitherR
import           Foreign                (FunPtr)
import qualified Foreign
import qualified System.Directory       as Dir
import qualified System.Environment     as Env
import           System.FilePath        ((</>), pathSeparator)
import qualified System.FilePath        as FP
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
            localNativeDirs <- forM dirs $ \dir -> findLocalNativeLibsDirs (localDepsDir </> dir)
            return $ map (\a -> localDepsDir </> a </> nativeLibs) dirs <> concat localNativeDirs

loadLibrary :: String -> IO Handle
loadLibrary ""          = cLibrary
loadLibrary namePattern = do
    projectDir <- return ""
    nativeDirs <- (nativeLibs :) <$> findLocalNativeLibsDirs projectDir
    let possibleNames = [ prefix ++ namePattern ++ extension | prefix    <- ["lib", ""]
                                                             , extension <- dynamicLibraryExtensions
                        ]
        projectNativeDirectories = [ nativeDir </> nativeLibraryProjectDir | nativeDir <- nativeDirs ]
        possiblePaths = [ dir </> name | dir  <- ("" : projectNativeDirectories)
                                       , name <- possibleNames
                        ]
    result <- runExceptT $ EitherR.runExceptRT $ do
        forM possiblePaths $ \path -> EitherR.ExceptRT $ ExceptT $ do
            loadRes <- tryAny $ nativeLoadLibrary path
            let errorDetails exc =
                    "loading \"" ++ path ++ "\" failed with: " ++ displayException exc
            return $ EitherR.fmapL errorDetails loadRes
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
        handle <- Win32.getModuleHandle ""
        exe    <- Win32.getModuleFileName handle
        return $ Dir.takeDirectory exe
    systemDirectory  <- Win32.getSystemDirectory
    windowsDirectory <- Win32.getWindowsDirectory
    currentDirectory <- Win32.getCurrentDirectory
    pathDirectories  <- do
        pathEnv <- FP.getSearchPath
        return $ FP.splitSearchPath pathEnv
    return $ exeDirectory
           : systemDirectory
           : windowsDirectory
           : currentDirectory
           : pathDirectories

#else

nativeLoadLibrary :: String -> IO Handle
nativeLoadLibrary library = Unix.dlopen library [Unix.RTLD_LAZY]

nativeLoadSymbol :: Handle -> String -> IO (FunPtr a)
nativeLoadSymbol handle symbol = Unix.dlsym handle symbol

cLibrary :: IO Handle
cLibrary = nativeLoadLibrary ""

nativeSearchPaths        :: [FilePath]
nativeSearchPaths = unsafePerformIO $ return []

dynamicLibraryExtensions :: [String]
nativeLibraryProjectDir  :: String
#if linux_HOST_OS
dynamicLibraryExtensions = [".so", ""]

nativeLibraryProjectDir = "linux"

#elif darwin_HOST_OS
dynamicLibraryExtensions = [".dylib", ""]

nativeLibraryProjectDir = "macos"
#endif

#endif