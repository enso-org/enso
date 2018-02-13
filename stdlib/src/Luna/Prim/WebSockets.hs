{-# LANGUAGE OverloadedStrings #-}

module Luna.Prim.WebSockets where

import qualified Prelude                    as P
import           Luna.Prelude               hiding (Text)
import           Luna.IR

import           Control.Concurrent         (MVar, readMVar, putMVar, newMVar, newEmptyMVar, modifyMVar_, forkIO, forkFinally)
import qualified Control.Exception          as Exception
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as ByteString
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as Text
import           Luna.Builtin.Prim          (toLunaValue, RuntimeRepOf, RuntimeRep (..))
import           Luna.Builtin.Data.Function (Function)
import           Luna.Builtin.Data.Module   (Imports)
import           Luna.Std.Builder           (LTp (..), makeFunctionIO, int)
import           Luna.Std.Finalizers        (registerFinalizer, FinalizersCtx)

import           Network.Socket             (withSocketsDo, PortNumber, SockAddr, Socket)
import qualified Network.Socket             as Socket
import qualified Network.WebSockets         as WebSocket
import qualified Wuss                       as WebSocket

data WSConnection = WSConnection { wsConn :: WebSocket.Connection, sem :: MVar () }
data WSServer     = WSServer     { wsServerSock   :: Socket
                                 , wsServerConns  :: MVar (Map SockAddr WSConnection)
                                 , wsServerMsg    :: MVar (WSConnection, Text)
                                 }

exports :: Imports -> FinalizersCtx -> IO (Map Name Function)
exports std finalizersCtx = do
    let noneT   = LCons "None"   []
        textT   = LCons "Text"   []
        binaryT = LCons "Binary" []
        boolT   = LCons "Bool"   []
        intT    = LCons "Int"    []

    let wrapWSConnection :: WebSocket.Connection -> IO WSConnection
        wrapWSConnection conn = WSConnection conn <$> newEmptyMVar

        unregisterConnection :: WSConnection -> IO ()
        unregisterConnection (WSConnection _ sem) = putMVar sem ()

        waitForConnectionClose :: WSConnection -> IO ()
        waitForConnectionClose (WSConnection _ sem) = readMVar sem

        runClient :: (Integral p) => P.String -> p -> P.String -> Bool -> WebSocket.ClientApp a -> IO a
        runClient host port path secure client = if secure
            then WebSocket.runSecureClient host (fromIntegral port :: PortNumber) path client
            else WebSocket.runClient       host (fromIntegral port :: Int)        path client

        primWebSocketConnectVal :: Text -> Integer -> Text -> Bool -> IO WSConnection
        primWebSocketConnectVal host port path secure = do
            connection <- newEmptyMVar :: IO (MVar WSConnection)
            let app :: WebSocket.ClientApp ()
                app conn = do
                    wsConn <- wrapWSConnection conn
                    putMVar connection wsConn
                    waitForConnectionClose wsConn
                    WebSocket.sendClose conn ("bye" :: Text)

            let strippedHost = Text.stripPrefix "ws://" host <|> Text.stripPrefix "wss://" host <|> Just host & fromJust
            forkIO $ withSocketsDo $ runClient (convert strippedHost) port (convert path) secure app
            wsConnection <- readMVar connection
            let cleanup = unregisterConnection wsConnection
            registerFinalizer finalizersCtx cleanup
            return wsConnection

    let wsConnectionT = LCons "WSConnection" []

    primWebSocketConnect <- makeFunctionIO (toLunaValue std primWebSocketConnectVal)
                                         [textT, intT, textT, boolT] wsConnectionT

    let primWebSocketReadVal :: WSConnection -> IO ByteString
        primWebSocketReadVal (WSConnection conn _) = WebSocket.receiveData conn
    primWebSocketRead <- makeFunctionIO (toLunaValue std primWebSocketReadVal)
                                      [wsConnectionT] binaryT

    let primWebSocketWriteVal :: WSConnection -> Text -> IO ()
        primWebSocketWriteVal (WSConnection conn _) s = WebSocket.sendTextData conn s
    primWebSocketWrite <- makeFunctionIO (toLunaValue std primWebSocketWriteVal)
                                       [wsConnectionT, textT] noneT

    let primWebSocketWriteBinVal :: WSConnection -> ByteString -> IO ()
        primWebSocketWriteBinVal (WSConnection conn _) s = WebSocket.sendBinaryData conn s
    primWebSocketWriteBin <- makeFunctionIO (toLunaValue std primWebSocketWriteVal)
                                       [wsConnectionT, binaryT] noneT

    let primWebSocketCloseVal :: WSConnection -> IO ()
        primWebSocketCloseVal = unregisterConnection
    primWebSocketClose <- makeFunctionIO (toLunaValue std primWebSocketCloseVal)
                                       [wsConnectionT] noneT

    let wsServerT = LCons "WSServer" []

        addWSServerConn :: SockAddr -> WSConnection -> MVar (Map SockAddr WSConnection) -> IO ()
        addWSServerConn saddr conn conns = modifyMVar_ conns $ return . Map.insert saddr conn

        initServer :: Text -> Integer -> IO WSServer
        initServer host port = do
            serverSock <- newEmptyMVar      :: IO (MVar Socket)
            conns      <- newMVar Map.empty :: IO (MVar (Map SockAddr WSConnection))
            msg        <- newEmptyMVar      :: IO (MVar (WSConnection, Text))
            sock       <- WebSocket.makeListenSocket (convert host) (int port)
            let wss = WSServer sock conns msg
            putMVar serverSock sock
            return wss

        receiveMessage :: WSServer -> WSConnection -> IO ()
        receiveMessage wss connection = do
            msg <- WebSocket.receiveData $ wsConn connection :: IO Text
            putMVar (wsServerMsg wss) (connection, msg)

        awaitConnections :: WSServer -> IO ()
        awaitConnections wss@(WSServer sock conns msg) = do
            mvar <- newEmptyMVar :: IO (MVar ())
            (flip forkFinally) (\_ -> putMVar mvar ()) $ forever $ do
                (sc, saddr) <- Socket.accept sock
                pc          <- WebSocket.makePendingConnection sc WebSocket.defaultConnectionOptions
                conn        <- WSConnection <$> WebSocket.acceptRequest pc <*> (newEmptyMVar :: IO (MVar ()))
                addWSServerConn saddr conn conns
                forkIO $ forever $ receiveMessage wss conn
            void $ readMVar mvar

        disconnectClients :: WSServer -> IO ()
        disconnectClients wss = do
            let conns = wsServerConns wss
            connMap <- readMVar conns
            forM_ connMap (\c -> WebSocket.sendClose (wsConn c) ("bye" :: Text))
            putMVar conns Map.empty

        primCreateWSServerVal :: Text -> Integer -> IO WSServer
        primCreateWSServerVal host port = do
            server <- initServer host port
            forkIO $ Exception.bracket_ (return ()) (disconnectClients server) (awaitConnections server)
            return server
    primCreateWSServer <- makeFunctionIO (toLunaValue std primCreateWSServerVal)
                                       [textT, intT] wsServerT

    let primWSSBroadcastTextVal :: WSServer -> Text -> IO WSServer
        primWSSBroadcastTextVal wss msg = do
            connMap <- readMVar $ wsServerConns wss
            forM_ connMap (\c -> WebSocket.sendTextData (wsConn c) msg)
            return wss
    primWSSBroadcastText <- makeFunctionIO (toLunaValue std primWSSBroadcastTextVal)
                                         [wsServerT, textT] wsServerT

    let primWSSBroadcastBinaryVal :: WSServer -> ByteString -> IO WSServer
        primWSSBroadcastBinaryVal wss msg = do
            connMap <- readMVar $ wsServerConns wss
            forM_ connMap (\c -> WebSocket.sendBinaryData (wsConn c) msg)
            return wss
    primWSSBroadcastBinary <- makeFunctionIO (toLunaValue std primWSSBroadcastBinaryVal)
                                           [wsServerT, binaryT] wsServerT

    let primWSSGetMessageVal :: WSServer -> IO (WSConnection, Text)
        primWSSGetMessageVal = readMVar . wsServerMsg
    primWSSGetMessage <- makeFunctionIO (toLunaValue std primWSSGetMessageVal)
                                       [wsServerT] (LCons "MVar" [textT])

    return $ Map.fromList [ ("primWebSocketConnect", primWebSocketConnect)
                          , ("primWebSocketRead", primWebSocketRead)
                          , ("primWebSocketWrite", primWebSocketWrite)
                          , ("primWebSocketWriteBin", primWebSocketWriteBin)
                          , ("primWebSocketClose", primWebSocketClose)
                          , ("primCreateWSServer", primCreateWSServer)
                          , ("primWSSBroadcastText", primWSSBroadcastText)
                          , ("primWSSBroadcastBinary", primWSSBroadcastBinary)
                          , ("primWSSGetMessage", primWSSGetMessage)
                          ]

type instance RuntimeRepOf WSConnection = AsNative "WSConnection"
type instance RuntimeRepOf WSServer     = AsNative "WSServer"
