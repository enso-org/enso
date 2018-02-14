{-# LANGUAGE OverloadedStrings #-}

module Luna.Prim.System where

import qualified Prelude                     as P
import           Luna.Prelude                hiding (Text)
import           Luna.IR

import qualified Control.Exception           as Exception
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text.Lazy              (Text)
import qualified Data.Text.Lazy              as Text

import           Luna.Builtin.Prim           (toLunaValue, ToLunaData, toLunaData, FromLunaData, fromLunaData, RuntimeRepOf, RuntimeRep (..))
import           Luna.Builtin.Data.LunaValue (LunaData (LunaObject), Object (..), Constructor (..), constructor, fields, tag, force')
import           Luna.Builtin.Data.LunaEff   (LunaEff, throw)
import           Luna.Builtin.Data.Function  (Function)
import           Luna.Builtin.Data.Module    (Imports, getObjectMethodMap)
import           Luna.Std.Builder            (LTp (..), makeFunctionIO, int, integer)

import           System.Exit                 (ExitCode (ExitFailure, ExitSuccess))
import           System.Process              (CreateProcess, ProcessHandle, StdStream (CreatePipe, Inherit, NoStream, UseHandle))
import qualified System.Process              as Process
import           GHC.IO.Exception            (IOErrorType (ResourceVanished), IOException (..))
import           GHC.IO.Handle               (Handle, BufferMode (..))
import qualified GHC.IO.Handle               as Handle
import           Foreign.C                   (ePIPE, Errno (Errno))

exports :: Imports -> IO (Map Name Function)
exports std = do
    let fileHandleT = LCons "FileHandle" []
        boolT       = LCons "Bool"       []
        noneT       = LCons "None"       []
        textT       = LCons "Text"       []

    let runProcessVal :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
        runProcessVal = Process.createProcess
    runProcess' <- makeFunctionIO (toLunaValue std runProcessVal) [ LCons "ProcessDescription" [] ] ( LCons "Process" [ LCons "Maybe" [ LCons "FileHandle" [] ]
                                                                                                                    , LCons "Maybe" [ LCons "FileHandle" [] ]
                                                                                                                    , LCons "Maybe" [ LCons "FileHandle" [] ]
                                                                                                                    , LCons "ProcessHandle" [] ] )
    let hIsOpenVal :: Handle -> IO Bool
        hIsOpenVal = Handle.hIsOpen
    hIsOpen' <- makeFunctionIO (toLunaValue std hIsOpenVal) [fileHandleT] boolT

    let hIsClosedVal :: Handle -> IO Bool
        hIsClosedVal = Handle.hIsClosed
    hIsClosed' <- makeFunctionIO (toLunaValue std hIsClosedVal) [fileHandleT] boolT

    let ignoreSigPipe :: IO () -> IO ()
        ignoreSigPipe = Exception.handle $ \e -> case e of
            IOError { ioe_type  = ResourceVanished
                    , ioe_errno = Just ioe }
                | Errno ioe == ePIPE -> return ()
            _ -> Exception.throwIO e
        hCloseVal :: Handle -> IO ()
        hCloseVal = ignoreSigPipe . Handle.hClose

    hClose' <- makeFunctionIO (toLunaValue std hCloseVal) [fileHandleT] noneT

    let hGetContentsVal :: Handle -> IO Text
        hGetContentsVal = fmap Text.pack . Handle.hGetContents
    hGetContents' <- makeFunctionIO (toLunaValue std hGetContentsVal) [fileHandleT] textT

    let hGetLineVal :: Handle -> IO Text
        hGetLineVal = fmap Text.pack . Handle.hGetLine
    hGetLine' <- makeFunctionIO (toLunaValue std hGetLineVal) [fileHandleT] textT

    let hPutTextVal :: Handle -> Text -> IO ()
        hPutTextVal h = Handle.hPutStr h . Text.unpack
    hPutText' <- makeFunctionIO (toLunaValue std hPutTextVal) [fileHandleT, textT] noneT

    let hFlushVal :: Handle -> IO ()
        hFlushVal = Handle.hFlush
    hFlush' <- makeFunctionIO (toLunaValue std hFlushVal) [fileHandleT] noneT

    let waitForProcessVal :: ProcessHandle -> IO ExitCode
        waitForProcessVal = Process.waitForProcess
    waitForProcess' <- makeFunctionIO (toLunaValue std waitForProcessVal) [LCons "ProcessHandle" []] (LCons "ExitCode" [])

    let hSetBufferingVal :: Handle -> BufferMode -> IO ()
        hSetBufferingVal = Handle.hSetBuffering
    hSetBuffering' <- makeFunctionIO (toLunaValue std hSetBufferingVal) [fileHandleT, LCons "BufferMode" []] noneT

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
                          ]

instance FromLunaData CreateProcess where
    fromLunaData v = let errorMsg = "Expected a ProcessDescription luna object, got unexpected constructor" in
        force' v >>= \case
            LunaObject obj -> case obj ^. constructor . fields of
                [command, args, stdin, stdout, stderr] -> do
                    p       <- Process.proc <$> fmap Text.unpack (fromLunaData command) <*> fmap (map Text.unpack) (fromLunaData args)
                    stdin'  <- fromLunaData stdin
                    stdout' <- fromLunaData stdout
                    stderr' <- fromLunaData stderr
                    return $ p { Process.std_in = stdin', Process.std_out = stdout', Process.std_err = stderr' }
                _ -> throw errorMsg
            _ -> throw errorMsg

instance FromLunaData StdStream where
    fromLunaData v = let errorMsg = "Expected a PipeRequest luna object, got unexpected constructor: " in
        force' v >>= \case
            LunaObject obj -> case obj ^. constructor . tag of
                "Inherit"    -> return Inherit
                "UseHandle"  -> UseHandle <$> (fromLunaData . head $ obj ^. constructor . fields)
                "CreatePipe" -> return CreatePipe
                "NoStream"   -> return NoStream
                c            -> throw (errorMsg <> convert c)
            c -> throw (errorMsg <> "Not a LunaObject")

instance FromLunaData BufferMode where
    fromLunaData v = let errorMsg = "Expected a BufferMode luna object, got unexpected constructor: " in
        force' v >>= \case
            LunaObject obj -> case obj ^. constructor . tag of
                "NoBuffering"    -> return NoBuffering
                "LineBuffering"  -> return LineBuffering
                "BlockBuffering" -> fmap (BlockBuffering . fmap int) . fromLunaData . head $ obj ^. constructor . fields
                c                -> throw (errorMsg <> convert c)
            c -> throw (errorMsg <> "Not a LunaObject")

instance FromLunaData ExitCode where
    fromLunaData v = force' v >>= \case
        LunaObject obj -> case obj ^. constructor . tag of
            "ExitSuccess" -> return ExitSuccess
            "ExitFailure" -> fmap (ExitFailure . int) . fromLunaData . head $ obj ^. constructor . fields
        _ -> throw "Expected a ExitCode luna object, got unexpected constructor"

instance ToLunaData ExitCode where
    toLunaData imps ec =
        let makeConstructor ExitSuccess     = Constructor "ExitSuccess" []
            makeConstructor (ExitFailure c) = Constructor "ExitFailure" [toLunaData imps $ integer c] in
        LunaObject $ Object (makeConstructor ec) $ getObjectMethodMap "ExitCode" imps

type instance RuntimeRepOf Handle        = AsNative "FileHandle"
type instance RuntimeRepOf ProcessHandle = AsNative "ProcessHandle"

instance {-# OVERLAPS #-} ToLunaData (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) where
    toLunaData imps (hin, hout, herr, ph) = LunaObject $ Object (Constructor "Process" [toLunaData imps hin, toLunaData imps hout, toLunaData imps herr, toLunaData imps ph]) $ getObjectMethodMap "Process" imps

