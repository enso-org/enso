{-# LANGUAGE UndecidableInstances #-}

module Luna.Compilation.Pass.Interpreter.Value where

import           Prelude.Luna
import           GHC.Prim      (Any)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Unsafe.Coerce
import           Control.Concurrent.MVar                         (newMVar, readMVar, takeMVar, putMVar)

type Ident    = String
data LunaM a  = Pure a | Monadic (IO a)
type Value    = LunaM Data
data Data     = Function (Data -> Value) | Data (Object ObjectData) | Boxed (Object Any)
data Object a = Object { _proto :: ClassDescription a
                       , _body  :: a
                       }
data ObjectData = ObjectData (Map Ident Value)
data ClassDescription a = ClassDescription (Map Ident (Method a))
data Method a = Method (Object a -> Value)
makeLenses  ''Object
makeWrapped ''Method
makeWrapped ''ClassDescription
makeWrapped ''ObjectData

toIO :: LunaM a -> IO a
toIO (Pure a)    = return a
toIO (Monadic a) = a

unsafeAppFun :: Value -> [Maybe Value] -> Value
unsafeAppFun f [] = f
unsafeAppFun f (Nothing : as) = do
    (Function func) <- f
    return . Function $ \x -> unsafeAppFun (func x) as
unsafeAppFun f (Just a  : as) = do
    (Function func) <- f
    a' <- a
    func a'

unsafeGetProperty :: String -> Value -> Value
unsafeGetProperty prop v = do
    (Boxed obj@(Object cls _)) <- v
    let method = unwrap . fromJust . Map.lookup prop . unwrap $ cls
    method obj

unsafeMakeAccessor :: String -> Value
unsafeMakeAccessor name = return . Function $ \d -> unsafeGetProperty name (return d)

makeBinder :: MonadIO m => Value -> m Value
makeBinder val = do
    var <- liftIO $ newMVar Nothing
    return $ do
        cached <- liftIO $ takeMVar var
        result <- case cached of
            Just d  -> return d
            Nothing -> (liftIO $ print "DUPING") >> val
        liftIO $ putMVar var (Just result)
        return result

instance Functor LunaM where
    fmap f (Pure a)    = Pure    $ f a
    fmap f (Monadic a) = Monadic $ fmap f a

instance Applicative LunaM where
    pure = Pure

    (Pure f) <*> (Pure a) = Pure $ f a
    f        <*> a        = Monadic $ toIO f <*> toIO a

instance Monad LunaM where
    (Pure a)    >>= f = f a
    (Monadic a) >>= f = Monadic $ fmap f a >>= toIO

instance MonadIO LunaM where
    liftIO = Monadic

intDesc :: ClassDescription Any
intDesc = ClassDescription $ Map.fromList
    [ ("+",     toMethodBoxed ((+)       :: Int -> Int -> Int))
    , ("*",     toMethodBoxed ((*)       :: Int -> Int -> Int))
    , ("succ",  toMethodBoxed (succ      :: Int -> Int))
    , ("times", toMethodBoxed (replicate :: Int -> Data -> [Data]))
    , ("pred",  toMethodBoxed ((\x -> putStrLn "PRED" >> return x) :: Int -> IO Int))
    ]

lstDesc :: ClassDescription Any
lstDesc = ClassDescription $ Map.fromList
    [ ("map",        toMethodBoxed (forM :: [Data] -> (Data -> Value) -> LunaM [Data]))
    , ("unsafeHead", toMethodBoxed (head :: [Data] -> Data))
    ]

dummyBox :: ClassDescription Any
dummyBox = ClassDescription Map.empty

class ToValue a where
    unsafeToValue :: a -> Value

class ToData a where
    unsafeToData :: a -> Data

class FromData a where
    unsafeFromData :: Data -> a

instance ToData Int where
    unsafeToData = Boxed . Object intDesc . unsafeCoerce

instance ToData Double where
    unsafeToData = Boxed . Object dummyBox . unsafeCoerce

instance ToData Rational where
    unsafeToData = Boxed . Object dummyBox . unsafeCoerce

instance ToData String where
    unsafeToData = Boxed . Object dummyBox . unsafeCoerce

instance {-# OVERLAPPABLE #-} ToData a => ToData [a] where
    unsafeToData = Boxed . Object lstDesc . unsafeCoerce . fmap unsafeToData

instance ToData Data where
    unsafeToData = id

instance (FromData a, ToValue b) => ToData (a -> b) where
    unsafeToData f = Function $ \x -> unsafeToValue $ f (unsafeFromData x)


instance {-# OVERLAPPABLE #-} ToData a => ToValue a where
    unsafeToValue = return . unsafeToData

instance ToData a => ToValue (LunaM a) where
    unsafeToValue = fmap unsafeToData

instance ToValue a => ToValue (IO a) where
    unsafeToValue a = Monadic $ a >>= toIO . unsafeToValue

instance ToValue Value where
    unsafeToValue = id


instance FromData Int where
    unsafeFromData (Boxed (Object _ i)) = unsafeCoerce i

instance FromData a => FromData [a] where
    unsafeFromData (Boxed (Object _ as)) = unsafeFromData <$> unsafeCoerce as

instance FromData (Data -> Value) where
    unsafeFromData (Function f) = f

instance FromData Data where
    unsafeFromData = id

toMethodBoxed :: forall a b. ToValue b => (a -> b) -> Method Any
toMethodBoxed f = Method $ \(Object _ x) -> unsafeToValue $ f (unsafeCoerce x :: a)

