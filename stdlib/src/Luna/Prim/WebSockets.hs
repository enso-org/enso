{-# LANGUAGE OverloadedStrings #-}

module Luna.Prim.WebSockets where

import           Prologue

import qualified Luna.IR as IR

import qualified Control.Exception           as Exception
import qualified Data.ByteString             as ByteString
import qualified Data.Map                    as Map
import qualified Data.Text                   as Text
import qualified Luna.Pass.Sourcing.Data.Def as Def
import qualified Luna.Runtime                as Luna
import qualified Luna.Std.Builder            as Builder
import qualified Network.Socket              as Socket
import qualified Network.WebSockets          as WebSocket
import qualified OCI.Data.Name               as Name
import qualified Wuss                        as WebSocket

import           Control.Concurrent         ( MVar, readMVar, putMVar
                                            , newMVar, newEmptyMVar
                                            , modifyMVar_, forkIO, forkFinally)
import           Data.ByteString            (ByteString)
import           Data.Map                   (Map)
import           Luna.Std.Builder           (LTp (..), makeFunctionIO, int)
import           Luna.Std.Finalizers        (registerFinalizer, FinalizersCtx)
import           Network.Socket             ( withSocketsDo, PortNumber
                                            , SockAddr, Socket)

type WSModule = "Std.WebSockets"

wsModule :: Name.Qualified
wsModule = Name.qualFromSymbol @WSModule


data WSConnection = WSConnection { wsConn :: WebSocket.Connection, sem :: MVar () }
data WSServer     = WSServer     { wsServerSock   :: Socket
                                 , wsServerConns  :: MVar (Map SockAddr WSConnection)
                                 , wsServerMsg    :: MVar (WSConnection, Text)
                                 }

exports :: forall graph m.
    ( Builder.StdBuilder graph m
    ) => FinalizersCtx -> m (Map IR.Name Def.Def)
exports finalizersCtx = do
    let noneT   = Builder.noneLT
        textT   = Builder.textLT
        binaryT = Builder.binaryLT
        boolT   = Builder.boolLT
        intT    = Builder.intLT

    let wrapWSConnection :: WebSocket.Connection -> IO WSConnection
        wrapWSConnection conn = WSConnection conn <$> newEmptyMVar

        unregisterConnection :: WSConnection -> IO ()
        unregisterConnection (WSConnection _ sem) = putMVar sem ()

        waitForConnectionClose :: WSConnection -> IO ()
        waitForConnectionClose (WSConnection _ sem) = readMVar sem

        runClient :: (Integral p) => String -> p -> String -> Bool -> WebSocket.ClientApp a -> IO a
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

            let strippedHost = Text.stripPrefix "ws://" host <|> Text.stripPrefix "wss://" host & fromJust host
            forkIO $ withSocketsDo $ runClient (convert strippedHost) port (convert path) secure app
            wsConnection <- readMVar connection
            let cleanup = unregisterConnection wsConnection
            registerFinalizer finalizersCtx cleanup
            return wsConnection

    let wsConnectionT = LCons wsModule "WSConnection" []

    primWebSocketConnect <- makeFunctionIO @graph (flip Luna.toValue primWebSocketConnectVal)
                                         [textT, intT, textT, boolT] wsConnectionT

    let primWebSocketReadVal :: WSConnection -> IO ByteString
        primWebSocketReadVal (WSConnection conn _) = WebSocket.receiveData conn
    primWebSocketRead <- makeFunctionIO @graph (flip Luna.toValue primWebSocketReadVal)
                                      [wsConnectionT] binaryT

    let primWebSocketWriteVal :: WSConnection -> Text -> IO ()
        primWebSocketWriteVal (WSConnection conn _) s = WebSocket.sendTextData conn s
    primWebSocketWrite <- makeFunctionIO @graph (flip Luna.toValue primWebSocketWriteVal)
                                       [wsConnectionT, textT] noneT

    let primWebSocketWriteBinVal :: WSConnection -> ByteString -> IO ()
        primWebSocketWriteBinVal (WSConnection conn _) s = WebSocket.sendBinaryData conn s
    primWebSocketWriteBin <- makeFunctionIO @graph (flip Luna.toValue primWebSocketWriteVal)
                                       [wsConnectionT, binaryT] noneT

    let primWebSocketCloseVal :: WSConnection -> IO ()
        primWebSocketCloseVal = unregisterConnection
    primWebSocketClose <- makeFunctionIO @graph (flip Luna.toValue primWebSocketCloseVal)
                                       [wsConnectionT] noneT

    let wsServerT = LCons wsModule "WSServer" []

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
    primCreateWSServer <- makeFunctionIO @graph (flip Luna.toValue primCreateWSServerVal)
                                       [textT, intT] wsServerT

    let primWSSBroadcastTextVal :: WSServer -> Text -> IO WSServer
        primWSSBroadcastTextVal wss msg = do
            connMap <- readMVar $ wsServerConns wss
            forM_ connMap (\c -> WebSocket.sendTextData (wsConn c) msg)
            return wss
    primWSSBroadcastText <- makeFunctionIO @graph (flip Luna.toValue primWSSBroadcastTextVal)
                                         [wsServerT, textT] wsServerT

    let primWSSBroadcastBinaryVal :: WSServer -> ByteString -> IO WSServer
        primWSSBroadcastBinaryVal wss msg = do
            connMap <- readMVar $ wsServerConns wss
            forM_ connMap (\c -> WebSocket.sendBinaryData (wsConn c) msg)
            return wss
    primWSSBroadcastBinary <- makeFunctionIO @graph (flip Luna.toValue primWSSBroadcastBinaryVal)
                                           [wsServerT, binaryT] wsServerT

    let primWSSGetMessageVal :: WSServer -> IO (WSConnection, Text)
        primWSSGetMessageVal = readMVar . wsServerMsg
    primWSSGetMessage <- makeFunctionIO @graph (flip Luna.toValue primWSSGetMessageVal)
                                       [wsServerT] (Builder.mvarLT textT)

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

type instance Luna.RuntimeRepOf WSConnection = Luna.AsNative ('Luna.ClassRep WSModule "WSConnection")
type instance Luna.RuntimeRepOf WSServer     = Luna.AsNative ('Luna.ClassRep WSModule "WSServer")
