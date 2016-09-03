module Luna.Compilation.Pass.Interpreter.StdScope where

import Prelude.Luna
import Luna.Compilation.Pass.Interpreter.Env
import Luna.Compilation.Pass.Interpreter.Value
import Data.List (sort, group)
import Control.Arrow ((&&&))
import Control.Monad.Fix (fix, mfix)
import Control.Concurrent
import Control.Exception (finally)
import Data.Time.Clock.POSIX
import Network.Socket hiding (Stream)
import qualified Network.Socket as Socket
import Control.Concurrent.MVar
import Text.Printf (printf)
import System.Cmd (system)
import System.IO

import qualified Data.Map as Map

stdScope = Scope $ Map.fromList
    [ ("id",        unsafeToValue (id :: Data -> Data))
    , ("const",     unsafeToValue (const :: Data -> Data -> Data))
    , ("readFile",  unsafeToValue readFile)
    , ("writeFile", unsafeToValue writeFile)
    , ("flip",      unsafeToValue (flip :: (Data -> Data -> Value) -> Data -> Data -> Value))
    , ("switch",    unsafeToValue ((\b t f -> if b then t else f) :: Bool -> Data -> Data -> Data))
    , ("singleton", unsafeToValue ((:[]) :: Data -> [Data]))
    , ("empty",     unsafeToValue ([] :: [Data]))
    , ("+",         unsafeToValue ((+) :: Int -> Int -> Int))
    , ("*",         unsafeToValue ((*) :: Int -> Int -> Int))
    , ("==",        unsafeToValue ((==) :: Int -> Int -> Bool))

    , ("mean",        unsafeToValue ((uncurry (/) . foldr (\e (s, c) -> (e + s, c + 1)) (0, 0)) :: [Double] -> Double))
    , ("differences", unsafeToValue ((\l -> zipWith (-) (drop 1 l) l) :: [Int] -> [Int]))
    , ("histogram",   unsafeToValue (map (head &&& length) . group . sort :: [Int] -> [(Int, Int)]))
    , ("primes",      unsafeToValue (primes :: Int -> [Int]))
    , ("pi",          unsafeToValue (pi :: Double))

    , ("fix",         unsafeToValue (fix :: (Data -> Data) -> Data))
    , ("app",         unsafeToValue (id :: Data -> Data))
    , ("prepend",     unsafeToValue ((:) :: Data -> [Data] -> [Data]))
    , ("comp2to2",    unsafeToValue ((\g h f x y -> f (g x y) (h x y)) :: (Data -> Data -> Data) -> (Data -> Data -> Data) -> (Data -> Data -> Data) -> Data -> Data -> Data))
    , ("time",        unsafeToValue time)
    , ("listen",      unsafeToValue listenSocket)
    , ("setLedColor", unsafeToValue setLedColor)
    , ("system",      unsafeToValue (\cmd -> void $ system cmd))
    ]

managingStream :: MVar Int -> MVar (Map.Map Int (Data -> IO ())) -> IO () -> Stream
managingStream nextId listeners destructor = Stream $ \listener -> do
    id <- takeMVar nextId
    putMVar nextId $ id + 1
    modifyMVar_ listeners $ return . Map.insert id listener
    return $ do
        lsts <- takeMVar listeners
        let newLsts = Map.delete id lsts
        when (Map.null newLsts) destructor
        putMVar listeners newLsts

time :: LunaM Stream
time = liftIO $ do
    listeners <- newMVar Map.empty
    let worker = do
          time <- round <$> getPOSIXTime
          lsts <- readMVar listeners
          mapM_ ($ unsafeToData (time :: Int)) $ Map.elems lsts
          threadDelay 1000000
          worker
    th     <- forkIO worker
    nextId <- newMVar 0
    return $ managingStream nextId listeners $ killThread th

runConnIn :: MySocket -> (Socket, SockAddr) -> Handle -> IO ()
runConnIn s (sock, addr) hdl = do
    let cleanUp = do
          thds <- withMVar (s ^. socketConnections) $ return . Map.lookup addr
          withJust thds $ \(_, i,o) -> do
              killThread o
              hClose hdl
              modifyMVar_ (s ^. socketConnections) $ return . Map.delete addr

        loop = fix $ \loop -> do
            line  <- hGetLine hdl
            lists <- readMVar $ s ^. socketListeners
            mapM_ ($ unsafeToData line) lists
            loop
    finally loop cleanUp

runConnOut :: MySocket -> (Socket, SockAddr) -> Handle -> IO ()
runConnOut s (sock, addr) hdl = do
    connChan <- dupChan $ s ^. socketPublishChan

    let cleanUp = do
          thds <- withMVar (s ^. socketConnections) $ return . Map.lookup addr
          withJust thds $ \(_, i,o) -> do
              killThread i
              hClose hdl
              modifyMVar_ (s ^. socketConnections) $ return . Map.delete addr

        loop = fix $ \loop -> do
            line <- readChan connChan
            hPutStrLn hdl line
            loop
    finally loop cleanUp


socketLoop :: MySocket -> IO ()
socketLoop s = do
    conn <- accept $ s ^. socketSocket
    let socket = fst conn
        addr   = snd conn
    hdl <- socketToHandle socket ReadWriteMode
    inThId  <- forkIO $ runConnIn  s conn hdl
    outThId <- forkIO $ runConnOut s conn hdl
    modifyMVar_ (s ^. socketConnections) $ return . Map.insert addr (hdl, inThId, outThId)
    socketLoop s

listenSocket :: Int -> LunaM MySocket
listenSocket port = liftIO $ do
    sock <- socket AF_INET Socket.Stream 0           -- create socket
    setSocketOption sock ReuseAddr 1          -- make socket immediately reusable - eases debugging.
    bind sock $ SockAddrInet (fromInteger $ fromIntegral $ port) iNADDR_ANY  -- listen on TCP port 4242.
    listen sock 2                             -- set a max of 2 queued connections

    chan        <- newChan
    listeners   <- newMVar Map.empty
    nextId      <- newMVar 0
    connections <- newMVar Map.empty

    let stream   = managingStream nextId listeners $ return ()

    thId <- forkIO $ socketLoop $ MySocket sock chan nextId listeners stream connections $ return ()
    let destructor = do
            killThread thId
            close sock
            conns <- readMVar connections
            forM_ (Map.toList conns) $ \(_, (hdl, th1, th2)) -> do
                killThread th1
                killThread th2
                hClose hdl

        mySocket = MySocket sock chan nextId listeners stream connections destructor

    return mySocket

primes :: Int -> [Int]
primes count = take count primes' where
    primes'   = 2 : filter isPrime [3, 5..]
    isPrime n = not $ any (\p -> n `rem` p == 0) $ takeWhile (\p -> p * p <= n) primes'

setLedColor :: Int -> Double -> Double -> Double -> IO ()
setLedColor ix r g b = do
    let vals = [ ix
               , (floor $ r * 255)
               , (floor $ g * 255)
               , (floor $ b * 255)
               ]
        line = (intercalate " " $ show <$> vals) <> "\n"
    writeFile "/dev/cu.usbmodemFD121" line
