{-# LANGUAGE UndecidableInstances #-}

module Luna.Compilation.Pass.Interpreter.Value where

import           Prelude.Luna
import           Prelude                    (error)
import           Text.Read                  (readEither)
import           GHC.Prim                   (Any)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Unsafe.Coerce
import           Data.List                  (sort, isInfixOf)
import           Data.Maybe                 (isJust, isNothing, listToMaybe, maybeToList)
import           Text.Printf                (printf)
import           Control.Monad.Except
import           Control.Concurrent         (ThreadId)
import           Control.Concurrent.MVar
import           Control.Concurrent.Chan    (Chan, writeChan)
import           Network.Socket             (Socket, SockAddr)
import           Control.Exception          (throw)
import           GHC.IO.Handle              (Handle)
import qualified System.Hardware.Serialport as SP
import qualified Data.ByteString.Char8      as B


type Ident    = String
data LunaM a  = Pure a | Monadic (ExceptT String IO a)
type Value    = LunaM Data
data Data     = Function (Data -> Value) | Data (Object ObjectData) | Boxed (Object Any)
data Object a = Object { _proto :: ClassDescription
                       , _body  :: a
                       }
data ObjectData       = ObjectData (Map Ident Value)
data ClassDescription = ClassDescription (Map Ident Method)
data Method           = Method (Data -> Value)

makeLenses  ''Object
makeWrapped ''Method
makeWrapped ''ClassDescription
makeWrapped ''ObjectData

toExceptIO :: LunaM a -> ExceptT String IO a
toExceptIO (Pure a)    = return a
toExceptIO (Monadic a) = a

toIO :: LunaM a -> IO a
toIO m = do
    m' <- runExceptT $ toExceptIO m
    case m' of
        Right a -> return a
        Left  e -> fail e

unsafeAppFun :: Value -> [Value] -> Value
unsafeAppFun = foldl unsafeAppArg where
    unsafeAppArg :: Value -> Value -> Value
    unsafeAppArg f a = do
        (Function func) <- f
        a' <- a
        func a'

unsafeGetProperty :: String -> Value -> Value
unsafeGetProperty prop v = do
    obj@(Boxed (Object cls _)) <- v
    let method = unwrap . fromJust . Map.lookup prop . unwrap $ cls
    method obj

unsafeMakeAccessor :: String -> Value
unsafeMakeAccessor name = return . Function $ \d -> unsafeGetProperty name (return d)

instance Functor LunaM where
    fmap f (Pure a)    = Pure    $ f a
    fmap f (Monadic a) = Monadic $ fmap f a

instance Applicative LunaM where
    pure = Pure

    (Pure f) <*> (Pure a) = Pure $ f a
    f        <*> a        = Monadic $ toExceptIO f <*> toExceptIO a

instance Monad LunaM where
    (Pure a)    >>= f = f a
    (Monadic a) >>= f = Monadic $ fmap f a >>= toExceptIO

instance MonadIO LunaM where
    liftIO = Monadic . liftIO

instance MonadError String LunaM where
    throwError = Monadic . throwError
    catchError (Pure a)    _       = Pure a
    catchError (Monadic v) handler = Monadic $ catchError v (toExceptIO . handler)

intDesc :: ClassDescription
intDesc = ClassDescription $ Map.fromList
    [ ("==",       toMethodBoxed ((==) :: Int -> Int -> Bool))
    , ("/=",       toMethodBoxed ((/=) :: Int -> Int -> Bool))
    , ("<",        toMethodBoxed ((<)  :: Int -> Int -> Bool))
    , ("<=",       toMethodBoxed ((<=) :: Int -> Int -> Bool))
    , (">",        toMethodBoxed ((>)  :: Int -> Int -> Bool))
    , (">=",       toMethodBoxed ((>=) :: Int -> Int -> Bool))
    , ("min",      toMethodBoxed (min  :: Int -> Int -> Int))
    , ("max",      toMethodBoxed (max  :: Int -> Int -> Int))

    , ("+",        toMethodBoxed ((+) :: Int -> Int -> Int))
    , ("*",        toMethodBoxed ((*) :: Int -> Int -> Int))
    , ("-",        toMethodBoxed ((-) :: Int -> Int -> Int))
    , ("/",        toMethodBoxed (intDiv :: Int -> Int -> LunaM Int))
    , ("%",        toMethodBoxed (intMod :: Int -> Int -> LunaM Int))
    , ("mod",      toMethodBoxed (intMod :: Int -> Int -> LunaM Int))
    , ("^",        toMethodBoxed ((^) :: Int -> Int -> Int))

    , ("negate",   toMethodBoxed (negate :: Int -> Int))
    , ("abs",      toMethodBoxed (abs    :: Int -> Int))
    , ("signum",   toMethodBoxed (signum :: Int -> Int))

    , ("pred",     toMethodBoxed (pred :: Int -> Int))
    , ("succ",     toMethodBoxed (succ :: Int -> Int))
    , ("even",     toMethodBoxed (even :: Int -> Bool))
    , ("odd",      toMethodBoxed (odd  :: Int -> Bool))

    , ("gcd",      toMethodBoxed (gcd :: Int -> Int -> Int))
    , ("lcm",      toMethodBoxed (lcm :: Int -> Int -> Int))

    , ("times",    toMethodBoxed (replicate  :: Int -> Data -> [Data]))
    , ("upto",     toMethodBoxed (enumFromTo :: Int -> Int  -> [Int]))

    , ("toDouble", toMethodBoxed (fromIntegral :: Int -> Double))
    , ("toString", toMethodBoxed (show         :: Int -> String))
    ]

intDiv :: Int -> Int -> LunaM Int
intDiv _ 0 = throwError "Error: Division by zero"
intDiv a b = return $ a `div` b

intMod :: Int -> Int -> LunaM Int
intMod _ 0 = throwError "Error: Division by zero"
intMod a b = return $ a `mod` b

lstDesc :: ClassDescription
lstDesc = ClassDescription $ Map.fromList
    [ ("+",       toMethodBoxed ((++)               :: [Data] -> [Data] -> [Data]))
    , ("append",  toMethodBoxed ((\l e -> l ++ [e]) :: [Data] -> Data -> [Data]))
    , ("prepend", toMethodBoxed (flip (:)           :: [Data] -> Data -> [Data]))
    , ("length",  toMethodBoxed (length             :: [Data] -> Int))
    , ("reverse", toMethodBoxed (reverse            :: [Data] -> [Data]))
    , ("take",    toMethodBoxed (flip take          :: [Data] -> Int -> [Data]))
    , ("drop",    toMethodBoxed (flip drop          :: [Data] -> Int -> [Data]))
    , ("sort",    toMethodBoxed (sort               :: [Int]  -> [Int]))

    , ("map",     toMethodBoxed (forM                            :: [Data] -> (Data -> Value) -> LunaM [Data]))
    , ("fold",    toMethodBoxed ((\l i f -> foldlM (flip f) i l) :: [Data] -> Data -> (Data -> Data -> Value) -> Value))
    , ("zip",     toMethodBoxed (flip zipWithM                   :: [Data] -> (Data -> Data -> Value) -> [Data] -> LunaM [Data]))
    , ("filter",  toMethodBoxed (flip filterM                    :: [Data] -> (Data -> LunaM Bool) -> LunaM [Data]))

    , ("head",       toMethodBoxed (listToMaybe :: [Data] -> Maybe Data))
    , ("unsafeHead", toMethodBoxed (head        :: [Data] -> Data))
    ]

maybeDesc :: ClassDescription Any
maybeDesc = ClassDescription $ Map.fromList
    [ ("fromMaybe",      toMethodBoxed (flip fromMaybe :: Maybe Data -> Data -> Data))
    , ("toList",         toMethodBoxed (maybeToList    :: Maybe Data -> [Data]))
    , ("isJust",         toMethodBoxed (isJust         :: Maybe Data -> Bool))
    , ("isNothing",      toMethodBoxed (isNothing      :: Maybe Data -> Bool))
    , ("unsafeFromJust", toMethodBoxed (fromJust       :: Maybe Data -> Data))
    ]

doubleDesc :: ClassDescription
doubleDesc = ClassDescription $ Map.fromList
    [ ("==",       toMethodBoxed ((==) :: Double -> Double -> Bool))
    , ("/=",       toMethodBoxed ((/=) :: Double -> Double -> Bool))
    , ("<",        toMethodBoxed ((<)  :: Double -> Double -> Bool))
    , ("<=",       toMethodBoxed ((<=) :: Double -> Double -> Bool))
    , (">",        toMethodBoxed ((>)  :: Double -> Double -> Bool))
    , (">=",       toMethodBoxed ((>=) :: Double -> Double -> Bool))
    , ("min",      toMethodBoxed (min  :: Double -> Double -> Double))
    , ("max",      toMethodBoxed (max  :: Double -> Double -> Double))

    , ("+",        toMethodBoxed ((+)  :: Double -> Double -> Double))
    , ("*",        toMethodBoxed ((*)  :: Double -> Double -> Double))
    , ("-",        toMethodBoxed ((-)  :: Double -> Double -> Double))
    , ("/",        toMethodBoxed ((/)  :: Double -> Double -> Double))
    , ("**",       toMethodBoxed ((**) :: Double -> Double -> Double))

    , ("negate",   toMethodBoxed (negate :: Double -> Double))
    , ("abs",      toMethodBoxed (abs    :: Double -> Double))
    , ("signum",   toMethodBoxed (signum :: Double -> Double))

    , ("round",    toMethodBoxed (round   :: Double -> Int))
    , ("ceiling",  toMethodBoxed (ceiling :: Double -> Int))
    , ("floor",    toMethodBoxed (floor   :: Double -> Int))

    , ("exp",      toMethodBoxed (exp  :: Double -> Double))
    , ("log",      toMethodBoxed (log  :: Double -> Double))
    , ("sqrt",     toMethodBoxed (sqrt :: Double -> Double))

    , ("sin",      toMethodBoxed (sin   :: Double -> Double))
    , ("cos",      toMethodBoxed (cos   :: Double -> Double))
    , ("tan",      toMethodBoxed (tan   :: Double -> Double))
    , ("asin",     toMethodBoxed (asin  :: Double -> Double))
    , ("acos",     toMethodBoxed (acos  :: Double -> Double))
    , ("atan",     toMethodBoxed (atan  :: Double -> Double))
    , ("sinh",     toMethodBoxed (sinh  :: Double -> Double))
    , ("cosh",     toMethodBoxed (cosh  :: Double -> Double))
    , ("tanh",     toMethodBoxed (tanh  :: Double -> Double))
    , ("asinh",    toMethodBoxed (asinh :: Double -> Double))
    , ("acosh",    toMethodBoxed (acosh :: Double -> Double))
    , ("atanh",    toMethodBoxed (atanh :: Double -> Double))

    , ("toString", toMethodBoxed (show :: Double -> String))
    , ("toStringFormat", toMethodBoxed (toStringFormat :: Double -> Int -> Int -> String))
    ]

boolDesc :: ClassDescription
boolDesc = ClassDescription $ Map.fromList
    [ ("==",       toMethodBoxed ((==) :: Bool -> Bool -> Bool))
    , ("/=",       toMethodBoxed ((/=) :: Bool -> Bool -> Bool))
    , ("<",        toMethodBoxed ((<)  :: Bool -> Bool -> Bool))
    , ("<=",       toMethodBoxed ((<=) :: Bool -> Bool -> Bool))
    , (">",        toMethodBoxed ((>)  :: Bool -> Bool -> Bool))
    , (">=",       toMethodBoxed ((>=) :: Bool -> Bool -> Bool))
    , ("min",      toMethodBoxed (min  :: Bool -> Bool -> Bool))
    , ("max",      toMethodBoxed (max  :: Bool -> Bool -> Bool))

    , ("&&",       toMethodBoxed ((&&) :: Bool -> Bool -> Bool))
    , ("||",       toMethodBoxed ((||) :: Bool -> Bool -> Bool))
    , ("not",      toMethodBoxed (not  :: Bool -> Bool))

    , ("toString", toMethodBoxed (show :: Bool -> String))
    ]

stringDesc :: ClassDescription
stringDesc = ClassDescription $ Map.fromList
    [ ("==",          toMethodBoxed ((==) :: String -> String -> Bool))
    , ("/=",          toMethodBoxed ((/=) :: String -> String -> Bool))
    , ("<",           toMethodBoxed ((<)  :: String -> String -> Bool))
    , ("<=",          toMethodBoxed ((<=) :: String -> String -> Bool))
    , (">",           toMethodBoxed ((>)  :: String -> String -> Bool))
    , (">=",          toMethodBoxed ((>=) :: String -> String -> Bool))
    , ("min",         toMethodBoxed (min  :: String -> String -> String))
    , ("max",         toMethodBoxed (max  :: String -> String -> String))

    , ("+",           toMethodBoxed ((++)        :: String -> String -> String))
    , ("length",      toMethodBoxed (length      :: String -> Int))
    , ("reverse",     toMethodBoxed (reverse     :: String -> String))
    , ("take",        toMethodBoxed (flip take   :: String -> Int -> String))
    , ("drop",        toMethodBoxed (flip drop   :: String -> Int -> String))
    , ("words",       toMethodBoxed (words       :: String -> [String]))
    , ("lines",       toMethodBoxed (lines       :: String -> [String]))
    , ("join",        toMethodBoxed (intercalate :: String -> [String] -> String))
    , ("isInfixOf",   toMethodBoxed (isInfixOf   :: String -> String -> Bool))

    , ("toString",    toMethodBoxed (id          :: String -> String))
    , ("parseInt",    toMethodBoxed (safeRead    :: String -> LunaM Int))
    , ("parseDouble", toMethodBoxed (safeRead    :: String -> LunaM Double))
    ]

newtype Stream = Stream ((Data -> IO ()) -> IO (IO ()))


data MySocket = MySocket { _socketSocket      :: Socket
                         , _socketPublishChan :: Chan String
                         , _socketNextId      :: MVar Int
                         , _socketListeners   :: MVar (Map.Map Int (Data -> IO ()))
                         , _socketStream      :: Stream
                         , _socketConnections :: MVar (Map.Map SockAddr (Handle, ThreadId, ThreadId))
                         , _destruct    :: IO ()
                         }

data LedRing = LedRing { _serialPort      :: SP.SerialPort
                       , _ledRingLastLed         :: MVar Int
                       , _ledRingDestruct :: IO ()
                       }

data Color = Color Double Double Double

socketDesc :: ClassDescription
socketDesc = ClassDescription $ Map.fromList
    [ ("write",       toMethodBoxed (socketWrite   :: MySocket -> String -> IO ()))
    , ("data",        toMethodBoxed (socketData    :: MySocket -> Stream))
    ]

ledRingDesc :: ClassDescription
ledRingDesc = ClassDescription $ Map.fromList
    [ ("setColor",     toMethodBoxed (setColor     :: LedRing -> Int -> Color -> IO ()))
    , ("setNextColor", toMethodBoxed (setNextColor :: LedRing ->        Color -> IO ()))
    ]

colorDesc :: ClassDescription
colorDesc = ClassDescription $ Map.fromList
    [ ]

dummyDesc :: ClassDescription
dummyDesc = ClassDescription Map.empty

class ToValue a where
    unsafeToValue :: a -> Value

class ToData a where
    unsafeToData :: a -> Data

class FromData a where
    unsafeFromData :: Data -> a

instance ToData Int where
    unsafeToData = Boxed . Object intDesc . unsafeCoerce

instance ToData Double where
    unsafeToData = Boxed . Object doubleDesc . unsafeCoerce

instance ToData Rational where
    unsafeToData = Boxed . Object dummyDesc . unsafeCoerce

instance ToData String where
    unsafeToData = Boxed . Object stringDesc . unsafeCoerce

instance ToData Bool where
    unsafeToData = Boxed . Object boolDesc . unsafeCoerce

instance ToData MySocket where
    unsafeToData = Boxed . Object socketDesc . unsafeCoerce

instance ToData LedRing where
    unsafeToData = Boxed . Object ledRingDesc . unsafeCoerce

instance ToData Color where
    unsafeToData = Boxed . Object colorDesc . unsafeCoerce

instance ToData () where
    unsafeToData = Boxed . Object dummyDesc . unsafeCoerce

instance (ToData a, ToData b) => ToData (a, b) where
    unsafeToData (a, b) = Boxed . Object dummyDesc $ unsafeCoerce (unsafeToData a, unsafeToData b)

instance {-# OVERLAPPABLE #-} ToData a => ToData [a] where
    unsafeToData = Boxed . Object lstDesc . unsafeCoerce . fmap unsafeToData

instance ToData a => ToData (Maybe a) where
    unsafeToData = Boxed . Object maybeDesc . unsafeCoerce . fmap unsafeToData

instance ToData Data where
    unsafeToData = id

instance (FromData a, ToValue b) => ToData (a -> b) where
    unsafeToData f = Function $ \x -> unsafeToValue $ f (unsafeFromData x)


instance {-# OVERLAPPABLE #-} ToData a => ToValue a where
    unsafeToValue = return . unsafeToData

instance ToData a => ToValue (LunaM a) where
    unsafeToValue = fmap unsafeToData

instance ToValue a => ToValue (IO a) where
    unsafeToValue a = Monadic $ liftIO a >>= toExceptIO . unsafeToValue

instance FromData Int where
    unsafeFromData (Boxed (Object _ i)) = unsafeCoerce i

instance FromData String where
    unsafeFromData (Boxed (Object _ s)) = unsafeCoerce s

instance FromData Bool where
    unsafeFromData (Boxed (Object _ s)) = unsafeCoerce s

instance FromData MySocket where
    unsafeFromData (Boxed (Object _ s)) = unsafeCoerce s

instance FromData LedRing where
    unsafeFromData (Boxed (Object _ s)) = unsafeCoerce s

instance FromData Color where
    unsafeFromData (Boxed (Object _ s)) = unsafeCoerce s

instance FromData () where
    unsafeFromData (Boxed (Object _ s)) = unsafeCoerce s

instance FromData Double where
    unsafeFromData (Boxed (Object _ s)) = unsafeCoerce s

instance (FromData a, FromData b) => FromData (a, b) where
    unsafeFromData (Boxed (Object _ s)) = let (a, b) = unsafeCoerce s in (unsafeFromData a, unsafeFromData b)

instance {-# OVERLAPPABLE #-} FromData a => FromData [a] where
    unsafeFromData (Boxed (Object _ as)) = unsafeFromData <$> unsafeCoerce as

instance FromData a => FromData (Maybe a) where
    unsafeFromData (Boxed (Object _ s)) = unsafeFromData <$> unsafeCoerce s

instance (ToData a, FromData b) => FromData (a -> LunaM b) where
    unsafeFromData (Function f) = fmap unsafeFromData . f . unsafeToData

instance {-# OVERLAPPABLE #-} (ToData a, FromData b) => FromData (a -> b) where
    unsafeFromData (Function f) a = case f (unsafeToData a) of
        Pure b -> unsafeFromData b

instance FromData Data where
    unsafeFromData = id

toMethodBoxed :: forall a b. (FromData a, ToValue b) => (a -> b) -> Method
toMethodBoxed f = Method $ \x -> unsafeToValue $ f (unsafeFromData x :: a)

toStringFormat :: Double -> Int -> Int -> String
toStringFormat v w dec = let format = "%" <> show w <> "." <> show dec <> "f" in printf format v

streamDesc :: ClassDescription
streamDesc = ClassDescription $ Map.fromList
    [ ("map",   toMethodBoxed mapStream)
    {-, ("accum", toMethodBoxed accumStream)-}
    ]

instance ToData Stream where
    unsafeToData = Boxed . Object streamDesc . unsafeCoerce

instance FromData Stream where
    unsafeFromData (Boxed (Object _ s)) = unsafeCoerce s

attachListener :: Stream -> (Data -> IO ()) -> IO (IO ())
attachListener = unwrap

mapStream :: Stream -> (Data -> Value) -> Stream
mapStream s f = Stream $ \l -> attachListener s $ (toIO . f) >=> l

safeRead :: Read a => String -> LunaM a
safeRead s = case readEither s of
    Left err -> throwError err
    Right v -> return v

{-accumStream :: Stream -> LunaM Stream-}
{-accumStream s = liftIO $ do-}
    {-dataVar <- newMVar []-}
    {-return $ Stream $ \l -> attachListener s $ \val -> do-}
        {-modifyMVar_ dataVar $ return . (val :)-}
        {-d <- readMVar dataVar-}
        {-l $ unsafeToData d-}

socketWrite :: MySocket -> String -> IO ()
socketWrite s payload = writeChan (_socketPublishChan s) payload

socketData :: MySocket -> Stream
socketData s = _socketStream s

setColor :: LedRing -> Int -> Color -> IO ()
setColor lr ix (Color r g b) = do
    let brightness = 64
    let vals = [ ix
               , floor $ r * brightness
               , floor $ g * brightness
               , floor $ b * brightness
               ]
        line = intercalate " " (show <$> vals) <> "\n"

    SP.send (_serialPort lr) $ B.pack line
    void $ swapMVar (_ledRingLastLed lr) ix

setNextColor :: LedRing -> Color -> IO ()
setNextColor  lr col = do
    lastLed <- readMVar $ _ledRingLastLed lr
    let nextLed =  (lastLed + 1) `mod` 16
    setColor lr nextLed col


makeLenses ''MySocket
makeLenses ''LedRing

makeWrapped ''Stream

{-zipStream :: Stream -> Stream -> (Data -> Data -> Value) ->-}
