{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Luna.Prim.System where

import Prologue

import qualified Control.Exception           as Exception
import qualified Data.Map                    as Map
import qualified Data.Text                   as Text
import qualified GHC.IO.Handle               as Handle
import qualified Luna.IR                     as IR
import qualified Luna.Runtime                as Luna
import qualified Luna.Std.Builder            as Builder
import qualified Luna.Pass.Sourcing.Data.Def as Def
import qualified OCI.Data.Name               as Name
import qualified System.Directory            as Dir
import qualified System.Process              as Process

import           Data.Map                    (Map)
import           Foreign.C                   (ePIPE, Errno (Errno))
import           GHC.IO.Exception            (IOErrorType (ResourceVanished), IOException (..))
import           GHC.IO.Handle               (Handle, BufferMode (..))
import           Luna.Std.Builder            (LTp (..), makeFunctionIO, int, integer, maybeLT)
import           System.Environment          (lookupEnv, setEnv)
import           System.Exit                 (ExitCode (ExitFailure, ExitSuccess))
import           System.Process              ( CreateProcess
                                             , ProcessHandle
                                             , StdStream (CreatePipe, Inherit, NoStream, UseHandle)
                                             )

data Process = Process (Maybe Handle) (Maybe Handle) (Maybe Handle) (ProcessHandle)
data System = Linux | MacOS | Windows deriving Show

currentHost :: System
#ifdef linux_HOST_OS
currentHost =  Linux
#elif darwin_HOST_OS
currentHost =  MacOS
#elif mingw32_HOST_OS
currentHost =  Windows
#else
Running on unsupported system.
#endif

type SystemModule = "Std.System"

systemModule :: Name.Qualified
systemModule = Name.qualFromSymbol @SystemModule

fileHandleLT :: LTp
fileHandleLT = LCons systemModule "FileHandle" []

processLT :: LTp
processLT = LCons systemModule "Process" []

processDescriptionLT :: LTp
processDescriptionLT = LCons systemModule "ProcessDescription" []

processHandleLT :: LTp
processHandleLT = LCons systemModule "ProcessHandle" []

exitCodeLT :: LTp
exitCodeLT = LCons systemModule "ExitCode" []

bufferModeLT :: LTp
bufferModeLT = LCons systemModule "BufferMode" []

platformLT :: LTp
platformLT = LCons systemModule "Platform" []

exports :: forall graph m.
    ( Builder.StdBuilder graph m
    ) => m (Map IR.Name Def.Def)
exports = do
    let fileHandleT = fileHandleLT
        boolT       = Builder.boolLT
        noneT       = Builder.noneLT
        textT       = Builder.textLT

    let runProcessVal :: CreateProcess -> IO Process
        runProcessVal p =
            fmap (\(hin, hout, herr, ph) -> Process hin hout herr ph)
                 (Process.createProcess p)
    runProcess' <- makeFunctionIO @graph (flip Luna.toValue runProcessVal) [processDescriptionLT] processLT
    let hIsOpenVal :: Handle -> IO Bool
        hIsOpenVal = Handle.hIsOpen
    hIsOpen' <- makeFunctionIO @graph (flip Luna.toValue hIsOpenVal) [fileHandleT] boolT

    let hIsClosedVal :: Handle -> IO Bool
        hIsClosedVal = Handle.hIsClosed
    hIsClosed' <- makeFunctionIO @graph (flip Luna.toValue hIsClosedVal) [fileHandleT] boolT

    let ignoreSigPipe :: IO () -> IO ()
        ignoreSigPipe = Exception.handle $ \e -> case e of
            IOError { ioe_type  = ResourceVanished
                    , ioe_errno = Just ioe }
                | Errno ioe == ePIPE -> return ()
            _ -> Exception.throwIO e
        hCloseVal :: Handle -> IO ()
        hCloseVal = ignoreSigPipe . Handle.hClose

    hClose' <- makeFunctionIO @graph (flip Luna.toValue hCloseVal) [fileHandleT] noneT

    let hGetContentsVal :: Handle -> IO Text
        hGetContentsVal = fmap Text.pack . Handle.hGetContents
    hGetContents' <- makeFunctionIO @graph (flip Luna.toValue hGetContentsVal) [fileHandleT] textT

    let hGetLineVal :: Handle -> IO Text
        hGetLineVal = fmap Text.pack . Handle.hGetLine
    hGetLine' <- makeFunctionIO @graph (flip Luna.toValue hGetLineVal) [fileHandleT] textT

    let hPutTextVal :: Handle -> Text -> IO ()
        hPutTextVal h = Handle.hPutStr h . Text.unpack
    hPutText' <- makeFunctionIO @graph (flip Luna.toValue hPutTextVal) [fileHandleT, textT] noneT

    let hFlushVal :: Handle -> IO ()
        hFlushVal = Handle.hFlush
    hFlush' <- makeFunctionIO @graph (flip Luna.toValue hFlushVal) [fileHandleT] noneT

    let waitForProcessVal :: ProcessHandle -> IO ExitCode
        waitForProcessVal = Process.waitForProcess
    waitForProcess' <- makeFunctionIO @graph (flip Luna.toValue waitForProcessVal) [processHandleLT] exitCodeLT

    let hSetBufferingVal :: Handle -> BufferMode -> IO ()
        hSetBufferingVal = Handle.hSetBuffering
    hSetBuffering' <- makeFunctionIO @graph (flip Luna.toValue hSetBufferingVal) [fileHandleT, bufferModeLT] noneT

    let hPlatformVal :: System
        hPlatformVal = currentHost
    hPlatform' <- makeFunctionIO @graph (flip Luna.toValue hPlatformVal) [] platformLT

    let hLookupEnv :: Text -> IO (Maybe Text)
        hLookupEnv vName = convert <<$>> lookupEnv (convert vName)
    hLookupEnv' <- makeFunctionIO @graph (flip Luna.toValue hLookupEnv) [textT] (maybeLT textT)

    let hSetEnv :: Text -> Text -> IO ()
        hSetEnv envName envVal = setEnv (convert envName) (convert envVal)
    hSetEnv' <- makeFunctionIO @graph (flip Luna.toValue hSetEnv) [textT, textT] noneT

    let hGetCurrentDirectory :: IO Text
        hGetCurrentDirectory = convert <$> Dir.getCurrentDirectory
    hGetCurrentDirectory' <- makeFunctionIO @graph (flip Luna.toValue hGetCurrentDirectory) [] textT

    return $ Map.fromList [ ("primRunProcess", runProcess')
                          , ("primHIsOpen", hIsOpen')
                          , ("primHIsClosed", hIsClosed')
                          , ("primHClose", hClose')
                          , ("primHGetContents", hGetContents')
                          , ("primHGetLine", hGetLine')
                          , ("primHPutText", hPutText')
                          , ("primHFlush", hFlush')
                          , ("primHSetBuffering", hSetBuffering')
                          , ("primWaitForProcess", waitForProcess')
                          , ("primPlatform", hPlatform')
                          , ("primLookupEnv", hLookupEnv')
                          , ("primSetEnv", hSetEnv')
                          , ("primGetCurrentDirectory", hGetCurrentDirectory')
                          ]

type instance Luna.RuntimeRepOf CreateProcess = Luna.AsClass CreateProcess ('Luna.ClassRep SystemModule "ProcessDescription")
instance Luna.FromObject CreateProcess where
    fromConstructor c = let errorMsg = "Expected a ProcessDescription luna object, got unexpected constructor" in
        case c of
            Luna.Constructor _ [command, args, stdin, stdout, stderr] -> do
                p       <- Process.proc <$> fmap Text.unpack (Luna.fromData command) <*> fmap (map Text.unpack) (Luna.fromData args)
                stdin'  <- Luna.fromData stdin
                stdout' <- Luna.fromData stdout
                stderr' <- Luna.fromData stderr
                return $ p { Process.std_in = stdin', Process.std_out = stdout', Process.std_err = stderr' }
            _ -> Luna.throw errorMsg

type instance Luna.RuntimeRepOf StdStream = Luna.AsClass StdStream ('Luna.ClassRep SystemModule "PipeRequest")
instance Luna.FromObject StdStream where
    fromConstructor c = let errorMsg = "Expected a PipeRequest luna object, got unexpected constructor: " in
        case c of
            Luna.Constructor "Inherit"    _   -> return Inherit
            Luna.Constructor "UseHandle"  [f] -> UseHandle <$> (Luna.fromData f)
            Luna.Constructor "CreatePipe" _   -> return CreatePipe
            Luna.Constructor "NoStream"   _   -> return NoStream
            Luna.Constructor r            _   -> Luna.throw (errorMsg <> convert r)

type instance Luna.RuntimeRepOf BufferMode = Luna.AsClass BufferMode ('Luna.ClassRep SystemModule "BufferMode")
instance Luna.FromObject BufferMode where
    fromConstructor c = let errorMsg = "Expected a BufferMode luna object, got unexpected constructor: " in
        case c of
            Luna.Constructor "NoBuffering"    _   -> return NoBuffering
            Luna.Constructor "LineBuffering"  _   -> return LineBuffering
            Luna.Constructor "BlockBuffering" [f] -> fmap (BlockBuffering . fmap int) . Luna.fromData $ f
            Luna.Constructor r                _   -> Luna.throw (errorMsg <> convert r)

type instance Luna.RuntimeRepOf ExitCode = Luna.AsClass ExitCode ('Luna.ClassRep SystemModule "ExitCode")
instance Luna.FromObject ExitCode where
    fromConstructor c = case c of
        Luna.Constructor "ExitSuccess" _   -> return ExitSuccess
        Luna.Constructor "ExitFailure" [f] -> fmap (ExitFailure . int) . Luna.fromData $ f
        _ -> Luna.throw "Expected a ExitCode luna object, got unexpected constructor"

instance Luna.ToObject ExitCode where
    toConstructor imps ec =
        let makeConstructor ExitSuccess     = Luna.Constructor "ExitSuccess" []
            makeConstructor (ExitFailure c) = Luna.Constructor "ExitFailure" [Luna.toData imps $ integer c] in
        makeConstructor ec

type instance Luna.RuntimeRepOf Handle        = Luna.AsNative ('Luna.ClassRep SystemModule "FileHandle")
type instance Luna.RuntimeRepOf ProcessHandle = Luna.AsNative ('Luna.ClassRep SystemModule "ProcessHandle")
type instance Luna.RuntimeRepOf Process = Luna.AsClass Process ('Luna.ClassRep SystemModule "Process")
type instance Luna.RuntimeRepOf System = Luna.AsClass System ('Luna.ClassRep SystemModule "Platform")

instance Luna.ToObject Process where
    toConstructor imps (Process hin hout herr ph) =
        Luna.Constructor "Process"
                         [ Luna.toData imps hin
                         , Luna.toData imps hout
                         , Luna.toData imps herr
                         , Luna.toData imps ph ]

instance Luna.ToObject System where
    toConstructor imps Windows = Luna.Constructor "Windows" []
    toConstructor imps Linux   = Luna.Constructor "Linux"   []
    toConstructor imps MacOS   = Luna.Constructor "MacOS"   []
