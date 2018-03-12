{-# LANGUAGE CPP #-}

module Luna.Prim.DynamicLinker where

import           Luna.Prelude

import           Control.Exception.Safe (throwString, tryAny)
import           Control.Monad.Except   (ExceptT(..), runExceptT)
import qualified Data.EitherR           as EitherR
import           Foreign                (FunPtr)
import qualified Foreign
import           System.FilePath        ((</>), pathSeparator)

#if mingw32_HOST_OS
import qualified System.Win32.DLL   as Win32
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


loadLibrary :: String -> IO Handle
loadLibrary ""          = cLibrary
loadLibrary namePattern = do
    projectDir <- return ""
    let possibleNames = [ prefix ++ namePattern ++ extension | prefix    <- ["lib", ""]
                                                             , extension <- dynamicLibraryExtensions
                        ]
        projectNativeDirectory = projectDir </> nativeLibs </> nativeLibraryProjectDir
        possiblePaths = [ dir </> name | dir  <- ["", projectNativeDirectory]
                                       , name <- possibleNames
                        ]
    result <- runExceptT $ EitherR.runExceptRT $ do
        forM possiblePaths $ \path -> do
            EitherR.ExceptRT $ ExceptT $ do
                loadRes <- tryAny $ nativeLoadLibrary path
                case loadRes of
                    Left exc -> do
                        return $ Left $ "loading \"" ++ path ++ "\" failed with: " ++ displayException exc
                    Right  h -> do
                        return $ Right h
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

#else

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
#elif darwin_HOST_OS
dynamicLibraryExtensions = [".dylib", ""]

nativeLibraryProjectDir = "macos"
#endif

#endif