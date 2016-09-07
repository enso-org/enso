module Luna.Compilation.Pass.Interpreter.StdScope where

import Prelude.Luna
import Luna.Compilation.Pass.Interpreter.Env
import Luna.Compilation.Pass.Interpreter.Value
import Data.List (sort, group)
import Control.Arrow ((&&&))
import Control.Monad.Fix (fix, mfix)
import Control.Concurrent
import Control.Exception (finally)
import Control.Monad.Except                         (throwError, ExceptT, runExceptT)
import Data.Time.Clock.POSIX
import Network.Socket hiding (Stream)
import qualified Network.Socket as Socket
import Control.Concurrent.MVar
import Text.Printf (printf)
import System.Cmd (system)
import System.Process (createProcess, proc)
import System.IO
import qualified System.Hardware.Serialport as SP
import Numeric (readHex)
import Data.Fixed (mod')

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
    , ("ledRing",     unsafeToValue ledRing)
    , ("system",      unsafeToValue (\cmd -> void $ system cmd))
    , ("say",         unsafeToValue (\what -> void $ createProcess (proc "say" [what])))
    , ("rgbColor",    unsafeToValue (\r g b -> Color r g b))
    , ("cssColor",    unsafeToValue cssColor)
    , ("hsvColor",    unsafeToValue hsvColor)
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

ledRing :: LunaM LedRing
ledRing = liftIO $ do
     let port = "/tmp/ledring"
     s       <- SP.openSerial port SP.defaultSerialSettings { SP.commSpeed = SP.CS9600 }
     lastLed <- newMVar 0
     return $ LedRing s lastLed $ SP.closeSerial s


colors = Map.fromList   [ ("black", normalizedColor 0 0 0)
                        , ("silver", normalizedColor 192 192 192)
                        , ("gray", normalizedColor 128 128 128)
                        , ("white", normalizedColor 255 255 255)
                        , ("maroon", normalizedColor 128 0 0)
                        , ("red", normalizedColor 255 0 0)
                        , ("purple", normalizedColor 128 0 128)
                        , ("fuchsia", normalizedColor 255 0 255)
                        , ("green", normalizedColor 0 128 0)
                        , ("lime", normalizedColor 0 255 0)
                        , ("olive", normalizedColor 128 128 0)
                        , ("yellow", normalizedColor 255 255 0)
                        , ("navy", normalizedColor 0 0 128)
                        , ("blue", normalizedColor 0 0 255)
                        , ("teal", normalizedColor 0 128 128)
                        , ("aqua", normalizedColor 0 255 255)
                        , ("aliceblue", normalizedColor 240 248 255)
                        , ("antiquewhite", normalizedColor 250 235 215)
                        , ("aqua", normalizedColor 0 255 255)
                        , ("aquamarine", normalizedColor 127 255 212)
                        , ("azure", normalizedColor 240 255 255)
                        , ("beige", normalizedColor 245 245 220)
                        , ("bisque", normalizedColor 255 228 196)
                        , ("black", normalizedColor 0 0 0)
                        , ("blanchedalmond", normalizedColor 255 235 205)
                        , ("blue", normalizedColor 0 0 255)
                        , ("blueviolet", normalizedColor 138 43 226)
                        , ("brown", normalizedColor 165 42 42)
                        , ("burlywood", normalizedColor 222 184 135)
                        , ("cadetblue", normalizedColor 95 158 160)
                        , ("chartreuse", normalizedColor 127 255 0)
                        , ("chocolate", normalizedColor 210 105 30)
                        , ("coral", normalizedColor 255 127 80)
                        , ("cornflowerblue", normalizedColor 100 149 237)
                        , ("cornsilk", normalizedColor 255 248 220)
                        , ("crimson", normalizedColor 220 20 60)
                        , ("cyan", normalizedColor 0 255 255)
                        , ("darkblue", normalizedColor 0 0 139)
                        , ("darkcyan", normalizedColor 0 139 139)
                        , ("darkgoldenrod", normalizedColor 184 134 11)
                        , ("darkgray", normalizedColor 169 169 169)
                        , ("darkgreen", normalizedColor 0 100 0)
                        , ("darkgrey", normalizedColor 169 169 169)
                        , ("darkkhaki", normalizedColor 189 183 107)
                        , ("darkmagenta", normalizedColor 139 0 139)
                        , ("darkolivegreen", normalizedColor 85 107 47)
                        , ("darkorange", normalizedColor 255 140 0)
                        , ("darkorchid", normalizedColor 153 50 204)
                        , ("darkred", normalizedColor 139 0 0)
                        , ("darksalmon", normalizedColor 233 150 122)
                        , ("darkseagreen", normalizedColor 143 188 143)
                        , ("darkslateblue", normalizedColor 72 61 139)
                        , ("darkslategray", normalizedColor 47 79 79)
                        , ("darkslategrey", normalizedColor 47 79 79)
                        , ("darkturquoise", normalizedColor 0 206 209)
                        , ("darkviolet", normalizedColor 148 0 211)
                        , ("deeppink", normalizedColor 255 20 147)
                        , ("deepskyblue", normalizedColor 0 191 255)
                        , ("dimgray", normalizedColor 105 105 105)
                        , ("dimgrey", normalizedColor 105 105 105)
                        , ("dodgerblue", normalizedColor 30 144 255)
                        , ("firebrick", normalizedColor 178 34 34)
                        , ("floralwhite", normalizedColor 255 250 240)
                        , ("forestgreen", normalizedColor 34 139 34)
                        , ("fuchsia", normalizedColor 255 0 255)
                        , ("gainsboro", normalizedColor 220 220 220)
                        , ("ghostwhite", normalizedColor 248 248 255)
                        , ("gold", normalizedColor 255 215 0)
                        , ("goldenrod", normalizedColor 218 165 32)
                        , ("gray", normalizedColor 128 128 128)
                        , ("green", normalizedColor 0 128 0)
                        , ("greenyellow", normalizedColor 173 255 47)
                        , ("grey", normalizedColor 128 128 128)
                        , ("honeydew", normalizedColor 240 255 240)
                        , ("hotpink", normalizedColor 255 105 180)
                        , ("indianred", normalizedColor 205 92 92)
                        , ("indigo", normalizedColor 75 0 130)
                        , ("ivory", normalizedColor 255 255 240)
                        , ("khaki", normalizedColor 240 230 140)
                        , ("lavender", normalizedColor 230 230 250)
                        , ("lavenderblush", normalizedColor 255 240 245)
                        , ("lawngreen", normalizedColor 124 252 0)
                        , ("lemonchiffon", normalizedColor 255 250 205)
                        , ("lightblue", normalizedColor 173 216 230)
                        , ("lightcoral", normalizedColor 240 128 128)
                        , ("lightcyan", normalizedColor 224 255 255)
                        , ("lightgoldenrodyellow", normalizedColor 250 250 210)
                        , ("lightgray", normalizedColor 211 211 211)
                        , ("lightgreen", normalizedColor 144 238 144)
                        , ("lightgrey", normalizedColor 211 211 211)
                        , ("lightpink", normalizedColor 255 182 193)
                        , ("lightsalmon", normalizedColor 255 160 122)
                        , ("lightseagreen", normalizedColor 32 178 170)
                        , ("lightskyblue", normalizedColor 135 206 250)
                        , ("lightslategray", normalizedColor 119 136 153)
                        , ("lightslategrey", normalizedColor 119 136 153)
                        , ("lightsteelblue", normalizedColor 176 196 222)
                        , ("lightyellow", normalizedColor 255 255 224)
                        , ("lime", normalizedColor 0 255 0)
                        , ("limegreen", normalizedColor 50 205 50)
                        , ("linen", normalizedColor 250 240 230)
                        , ("magenta", normalizedColor 255 0 255)
                        , ("maroon", normalizedColor 128 0 0)
                        , ("mediumaquamarine", normalizedColor 102 205 170)
                        , ("mediumblue", normalizedColor 0 0 205)
                        , ("mediumorchid", normalizedColor 186 85 211)
                        , ("mediumpurple", normalizedColor 147 112 219)
                        , ("mediumseagreen", normalizedColor 60 179 113)
                        , ("mediumslateblue", normalizedColor 123 104 238)
                        , ("mediumspringgreen", normalizedColor 0 250 154)
                        , ("mediumturquoise", normalizedColor 72 209 204)
                        , ("mediumvioletred", normalizedColor 199 21 133)
                        , ("midnightblue", normalizedColor 25 25 112)
                        , ("mintcream", normalizedColor 245 255 250)
                        , ("mistyrose", normalizedColor 255 228 225)
                        , ("moccasin", normalizedColor 255 228 181)
                        , ("navajowhite", normalizedColor 255 222 173)
                        , ("navy", normalizedColor 0 0 128)
                        , ("oldlace", normalizedColor 253 245 230)
                        , ("olive", normalizedColor 128 128 0)
                        , ("olivedrab", normalizedColor 107 142 35)
                        , ("orange", normalizedColor 255 165 0)
                        , ("orangered", normalizedColor 255 69 0)
                        , ("orchid", normalizedColor 218 112 214)
                        , ("palegoldenrod", normalizedColor 238 232 170)
                        , ("palegreen", normalizedColor 152 251 152)
                        , ("paleturquoise", normalizedColor 175 238 238)
                        , ("palevioletred", normalizedColor 219 112 147)
                        , ("papayawhip", normalizedColor 255 239 213)
                        , ("peachpuff", normalizedColor 255 218 185)
                        , ("peru", normalizedColor 205 133 63)
                        , ("pink", normalizedColor 255 192 203)
                        , ("plum", normalizedColor 221 160 221)
                        , ("powderblue", normalizedColor 176 224 230)
                        , ("purple", normalizedColor 128 0 128)
                        , ("red", normalizedColor 255 0 0)
                        , ("rosybrown", normalizedColor 188 143 143)
                        , ("royalblue", normalizedColor 65 105 225)
                        , ("saddlebrown", normalizedColor 139 69 19)
                        , ("salmon", normalizedColor 250 128 114)
                        , ("sandybrown", normalizedColor 244 164 96)
                        , ("seagreen", normalizedColor 46 139 87)
                        , ("seashell", normalizedColor 255 245 238)
                        , ("sienna", normalizedColor 160 82 45)
                        , ("silver", normalizedColor 192 192 192)
                        , ("skyblue", normalizedColor 135 206 235)
                        , ("slateblue", normalizedColor 106 90 205)
                        , ("slategray", normalizedColor 112 128 144)
                        , ("slategrey", normalizedColor 112 128 144)
                        , ("snow", normalizedColor 255 250 250)
                        , ("springgreen", normalizedColor 0 255 127)
                        , ("steelblue", normalizedColor 70 130 180)
                        , ("tan", normalizedColor 210 180 140)
                        , ("teal", normalizedColor 0 128 128)
                        , ("thistle", normalizedColor 216 191 216)
                        , ("tomato", normalizedColor 255 99 71)
                        , ("turquoise", normalizedColor 64 224 208)
                        , ("violet", normalizedColor 238 130 238)
                        , ("wheat", normalizedColor 245 222 179)
                        , ("white", normalizedColor 255 255 255)
                        , ("whitesmoke", normalizedColor 245 245 245)
                        , ("yellow", normalizedColor 255 255 0)
                        , ("yellowgreen", normalizedColor 154 205 50)
                        ]
normalizedColor :: Int -> Int -> Int -> Color
normalizedColor r g b = Color ((fromIntegral r) / 255.0) ((fromIntegral g) / 255.0) ((fromIntegral b) / 255.0)

cssColor :: String -> LunaM Color
cssColor ('#':hexCol) = handle hexCol where
    handle hs | length hs <= 6
      = case hs of
        [a,b,c,d,e,f] -> normalizedColor <$> (hex a b) <*> (hex c d) <*> (hex e f)
        [a,b,c]       -> normalizedColor <$> (hex a a) <*> (hex b b) <*> (hex c c)
        _             -> throwError $ "Could not parse color"
    handle _ = throwError $ "Could not parse color"
    hex a b = case readHex [a,b] of
                [(h,"")] -> return h
                _        -> throwError $ "could not parse as a hex value " ++ [a,b]


cssColor name = maybe (throwError "Unknown color") return $ Map.lookup name colors


hsvColor :: Double -> Double -> Double -> Color
hsvColor h s v = case hi of
    0 -> Color v t p
    1 -> Color q v p
    2 -> Color p v t
    3 -> Color p q v
    4 -> Color t p v
    5 -> Color v p q
 where
  hi = floor (h/60) `mod` 6
  f = (h/60) `mod'` 1
  p = v*(1-s)
  q = v*(1-f*s)
  t = v*(1-(1-f)*s)
