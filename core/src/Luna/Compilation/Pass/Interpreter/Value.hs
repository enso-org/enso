{-# LANGUAGE UndecidableInstances #-}

module Luna.Compilation.Pass.Interpreter.Value where

import           Prelude.Luna
import           GHC.Prim      (Any)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Unsafe.Coerce
import           Data.List (sort)
import           Text.Printf        (printf)

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
    , ("/",        toMethodBoxed (div :: Int -> Int -> Int))
    , ("%",        toMethodBoxed (mod :: Int -> Int -> Int))
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

lstDesc :: ClassDescription Any
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
    ]

doubleDesc :: ClassDescription Any
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

boolDesc :: ClassDescription Any
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

stringDesc :: ClassDescription Any
stringDesc = ClassDescription $ Map.fromList
    [ ("==",       toMethodBoxed ((==) :: String -> String -> Bool))
    , ("/=",       toMethodBoxed ((/=) :: String -> String -> Bool))
    , ("<",        toMethodBoxed ((<)  :: String -> String -> Bool))
    , ("<=",       toMethodBoxed ((<=) :: String -> String -> Bool))
    , (">",        toMethodBoxed ((>)  :: String -> String -> Bool))
    , (">=",       toMethodBoxed ((>=) :: String -> String -> Bool))
    , ("min",      toMethodBoxed (min  :: String -> String -> String))
    , ("max",      toMethodBoxed (max  :: String -> String -> String))

    , ("+",        toMethodBoxed ((++)        :: String -> String -> String))
    , ("length",   toMethodBoxed (length      :: String -> Int))
    , ("reverse",  toMethodBoxed (reverse     :: String -> String))
    , ("take",     toMethodBoxed (flip take   :: String -> Int -> String))
    , ("drop",     toMethodBoxed (flip drop   :: String -> Int -> String))
    , ("words",    toMethodBoxed (words       :: String -> [String]))
    , ("lines",    toMethodBoxed (lines       :: String -> [String]))
    , ("join",     toMethodBoxed (intercalate :: String -> [String] -> String))

    , ("toString", toMethodBoxed (id          :: String -> String))
    ]

dummyDesc :: ClassDescription Any
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

instance (ToData a, ToData b) => ToData (a, b) where
    unsafeToData (a, b) = Boxed . Object dummyDesc $ unsafeCoerce (unsafeToData a, unsafeToData b)

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

instance FromData Int where
    unsafeFromData (Boxed (Object _ i)) = unsafeCoerce i

instance FromData String where
    unsafeFromData (Boxed (Object _ s)) = unsafeCoerce s

instance FromData Bool where
    unsafeFromData (Boxed (Object _ s)) = unsafeCoerce s

instance FromData Double where
    unsafeFromData (Boxed (Object _ s)) = unsafeCoerce s

instance (FromData a, FromData b) => FromData (a, b) where
    unsafeFromData (Boxed (Object _ s)) = let (a, b) = unsafeCoerce s in (unsafeFromData a, unsafeFromData b)

instance {-# OVERLAPPABLE #-} FromData a => FromData [a] where
    unsafeFromData (Boxed (Object _ as)) = unsafeFromData <$> unsafeCoerce as

instance (ToData a, FromData b) => FromData (a -> LunaM b) where
    unsafeFromData (Function f) = fmap unsafeFromData . f . unsafeToData

instance {-# OVERLAPPABLE #-} (ToData a, FromData b) => FromData (a -> b) where
    unsafeFromData (Function f) a = case f (unsafeToData a) of
        Pure b -> unsafeFromData b

instance FromData Data where
    unsafeFromData = id

toMethodBoxed :: forall a b. ToValue b => (a -> b) -> Method Any
toMethodBoxed f = Method $ \(Object _ x) -> unsafeToValue $ f (unsafeCoerce x :: a)

toStringFormat :: Double -> Int -> Int -> String
toStringFormat v w dec = let format = "%" <> show w <> "." <> show dec <> "f" in printf format v
