{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}

module Luna.Builtin.Prim where

import Prologue_old                    hiding (force, Text)

import Control.Concurrent
import Control.Concurrent.MVar
import qualified Control.Exception.Safe as Exception
import Data.ByteString.Lazy        hiding (head)
import Data.Maybe                  (fromMaybe, maybeToList)
import Data.Text.Lazy              hiding (head)
import GHC.Exts                    (Any)
import Luna.IR                     (Name)
import Luna.Builtin.Data.LunaEff   (LunaEff, performIO, runError, runIO, throw)
import Luna.Builtin.Data.LunaValue
import Luna.Builtin.Data.Module
import Luna.Builtin.Data.Stream    (Stream)
import GHC.TypeLits
import Data.Proxy
import Unsafe.Coerce

data RuntimeRep = AsNative Symbol
                | AsClass  Symbol Type

type family GetRepClassName (a :: RuntimeRep) = (k :: Symbol) where
    GetRepClassName (AsNative s)  = s
    GetRepClassName (AsClass s _) = s

type family IsNative (a :: RuntimeRep) where
    IsNative (AsNative _) = True

type family IsClass (a :: RuntimeRep) where
    IsClass (AsClass _ _) = True

type family RuntimeRepOf a = (k :: RuntimeRep) | k -> a

----------------------------
-- === Native objects === --
----------------------------

type IsBoxed a = (IsNative (RuntimeRepOf a) ~ True, KnownSymbol (GetRepClassName (RuntimeRepOf a)))

classNameOf :: forall a. (KnownSymbol (GetRepClassName (RuntimeRepOf a))) => Name
classNameOf = convert $ symbolVal $ Proxy @(GetRepClassName (RuntimeRepOf a))

toBoxed :: forall a. IsBoxed a => Imports -> a -> Object Any
toBoxed imps i = Object (unsafeCoerce i) $ getObjectMethodMap (classNameOf @a) imps

fromBoxed :: IsBoxed a => Object Any -> a
fromBoxed (Object s _) = unsafeCoerce s

--------------------------
-- === IsLunaObject === --
--------------------------

class (KnownSymbol (GetRepClassName (RuntimeRepOf a)), IsClass (RuntimeRepOf a) ~ True) => ToLunaObject a where
    toConstructor   :: Imports -> a -> Constructor

class (IsClass (RuntimeRepOf a) ~ True) => FromLunaObject a where
    fromConstructor :: Constructor -> LunaEff a

fromLunaObject :: FromLunaObject a => Object Constructor -> LunaEff a
fromLunaObject (Object a _) = fromConstructor a

toLunaObject :: forall a. ToLunaObject a => Imports -> a -> Object Constructor
toLunaObject imps a = Object (toConstructor imps a) $ getObjectMethodMap (classNameOf @a) imps

---------------------------------------
-- === ToLunaData / FromLunaData === --
---------------------------------------

class FromLunaData' (k :: RuntimeRep) a where
    fromLunaData' :: LunaData -> LunaEff a

instance (RuntimeRepOf a ~ AsNative n, KnownSymbol n) => FromLunaData' (AsNative n) a where
    fromLunaData' (LunaBoxed a)    = return $ fromBoxed a
    fromLunaData' (LunaThunk a)    = a >>= fromLunaData' @(AsNative n)
    fromLunaData' (LunaSusp  a)    = a >>= fromLunaData' @(AsNative n)
    fromLunaData' (LunaObject _)   = throw "Expected a Boxed value, got an Object"
    fromLunaData' (LunaFunction _) = throw "Expected a Boxed value, got a Function"
    fromLunaData' LunaNoValue      = throw "Expected a Boxed value, got a Compilation Error"
    fromLunaData' (LunaError e)    = throw e

instance (RuntimeRepOf a ~ AsClass n a, FromLunaObject a) => FromLunaData' (AsClass n a) a where
    fromLunaData' (LunaThunk a)             = a >>= fromLunaData' @(AsClass n a)
    fromLunaData' (LunaSusp  a)             = a >>= fromLunaData' @(AsClass n a)
    fromLunaData' (LunaObject a)            = fromLunaObject a
    fromLunaData' (LunaBoxed _)             = throw "Expected an Object, got a native value"
    fromLunaData' (LunaFunction _)          = throw "Expected an Object, got a Function"
    fromLunaData' LunaNoValue               = throw "Expected an Object, got a Compilation Error"
    fromLunaData' (LunaError e)             = throw e

type IsLunaData a = (FromLunaData a, ToLunaData a)

class FromLunaData a where
    fromLunaData :: LunaData -> LunaEff a

instance {-# OVERLAPPABLE #-} FromLunaData' (RuntimeRepOf a) a => FromLunaData a where
    fromLunaData = fromLunaData' @(RuntimeRepOf a)

instance FromLunaData LunaData where
    fromLunaData = return

class ToLunaData' (k :: RuntimeRep) a where
    toLunaData' :: Imports -> a -> LunaData

instance (RuntimeRepOf a ~ AsNative n, KnownSymbol n) => ToLunaData' (AsNative n) a where
    toLunaData' = LunaBoxed .: toBoxed

instance (RuntimeRepOf a ~ AsClass n a, ToLunaObject a) => ToLunaData' (AsClass n a) a where
    toLunaData' = LunaObject .: toLunaObject

class ToLunaData a where
    toLunaData :: Imports -> a -> LunaData

instance {-# OVERLAPPABLE #-} ToLunaData' (RuntimeRepOf a) a => ToLunaData a where
    toLunaData = toLunaData' @(RuntimeRepOf a)

instance ToLunaData LunaData where
    toLunaData _ = id

instance ToLunaData Bool where
    toLunaData imps b = LunaObject $ Object (Constructor (if b then "True" else "False") []) $ getObjectMethodMap "Bool" imps

instance {-# OVERLAPPABLE #-} ToLunaData a => ToLunaData [a] where
    toLunaData imps []       = LunaObject $ Object (Constructor "Empty"   [])                                      $ getObjectMethodMap "List" imps
    toLunaData imps (a : as) = LunaObject $ Object (Constructor "Prepend" [toLunaData imps a, toLunaData imps as]) $ getObjectMethodMap "List" imps

instance ToLunaData Text where
    toLunaData = LunaBoxed .: toBoxed

instance (FromLunaData a, FromLunaData b) => FromLunaData (a, b) where
    fromLunaData v = let errorMsg = "Expected a Tuple2 luna object, got an unexpected constructor" in
        force' v >>= \case
            LunaObject obj -> case obj ^. constructor . tag of
                "Tuple2" -> (,) <$> fromLunaData a <*> fromLunaData b where [a, b] = obj ^. constructor . fields
                _        ->  throw errorMsg
            _              -> throw errorMsg

instance (FromLunaData a, FromLunaData b, FromLunaData c) => FromLunaData (a, b, c) where
    fromLunaData v = let errorMsg = "Expected a Tuple3 luna object, got an unexpected constructor" in
        force' v >>= \case
            LunaObject obj -> case obj ^. constructor . tag of
                "Tuple3" -> (,,) <$> fromLunaData a <*> fromLunaData b <*> fromLunaData c where [a, b, c] = obj ^. constructor . fields
                _        -> throw errorMsg
            _              -> throw errorMsg

instance (FromLunaData a, FromLunaData b, FromLunaData c, FromLunaData d) => FromLunaData (a, b, c, d) where
    fromLunaData v = let errorMsg = "Expected a Tuple4 luna object, got an unexpected constructor" in
        force' v >>= \case
            LunaObject obj -> case obj ^. constructor . tag of
                "Tuple4" -> (,,,) <$> fromLunaData a <*> fromLunaData b <*> fromLunaData c <*> fromLunaData d where [a, b, c, d] = obj ^. constructor . fields
                _        -> throw errorMsg
            _              -> throw errorMsg

instance (ToLunaData a, ToLunaData b) => ToLunaData (a, b) where
    toLunaData imps (a, b) = LunaObject $ Object (Constructor "Tuple2" [toLunaData imps a, toLunaData imps b]) $ getObjectMethodMap "Tuple2" imps

instance (ToLunaData a, ToLunaData b, ToLunaData c) => ToLunaData (a, b, c) where
    toLunaData imps (a, b, c) = LunaObject $ Object (Constructor "Tuple3" [toLunaData imps a, toLunaData imps b, toLunaData imps c]) $ getObjectMethodMap "Tuple3" imps

instance (ToLunaData a, ToLunaData b, ToLunaData c, ToLunaData d) => ToLunaData (a, b, c, d) where
    toLunaData imps (a, b, c, d) = LunaObject $ Object (Constructor "Tuple4" [toLunaData imps a, toLunaData imps b, toLunaData imps c, toLunaData imps d]) $ getObjectMethodMap "Tuple4" imps

instance ToLunaData a => ToLunaData (Maybe a) where
    toLunaData imps Nothing  = LunaObject $ Object (Constructor "Nothing" [])               $ getObjectMethodMap "Maybe" imps
    toLunaData imps (Just a) = LunaObject $ Object (Constructor "Just" [toLunaData imps a]) $ getObjectMethodMap "Maybe" imps

instance (ToLunaData a, ToLunaData b) => ToLunaData (Either a b) where
    toLunaData imps (Left  a) = LunaObject $ Object (Constructor "Left"  [toLunaData imps a]) $ getObjectMethodMap "Either" imps
    toLunaData imps (Right b) = LunaObject $ Object (Constructor "Right" [toLunaData imps b]) $ getObjectMethodMap "Either" imps

instance ToLunaData () where
    toLunaData imps _ = LunaObject $ Object (Constructor "None" []) $ getObjectMethodMap "None" imps

instance FromLunaData () where
    fromLunaData v = force' v >> return ()

instance FromLunaData Bool where
    fromLunaData v = force' v >>= \case
        LunaObject obj -> return $ obj ^. constructor . tag == "True"
        _              -> throw "Expected a bool luna value, got unexpected constructor"

instance FromLunaData a => FromLunaData [a] where
    fromLunaData v = force' v >>= \case
        LunaObject obj -> if obj ^. constructor . tag == "Empty"
            then return []
            else let [x, t] = obj ^. constructor . fields in (:) <$> fromLunaData x <*> fromLunaData t
        _              -> throw "Expected a List luna value, got unexpected constructor"

instance FromLunaData a => FromLunaData (Maybe a) where
    fromLunaData v = force' v >>= \case
        LunaObject obj -> if obj ^. constructor . tag == "Nothing"
            then return Nothing
            else Just <$> (fromLunaData . head $ obj ^. constructor . fields)
        _              -> throw "Expected a Maybe luna value, got unexpected constructor"
-------------------------
-- === ToLunaValue === --
-------------------------

class ToLunaValue a where
    toLunaValue :: Imports -> a -> LunaValue

instance {-# OVERLAPPABLE #-} ToLunaData a => ToLunaValue a where
    toLunaValue = return .: toLunaData

-----------------------
-- === Instances === --
-----------------------

type instance RuntimeRepOf Integer         = AsNative "Int"
type instance RuntimeRepOf Double          = AsNative "Real"
type instance RuntimeRepOf Text            = AsNative "Text"
type instance RuntimeRepOf ByteString      = AsNative "Binary"
type instance RuntimeRepOf (MVar LunaData) = AsNative "MVar"


type instance RuntimeRepOf () = AsClass "None" ()
instance FromLunaObject () where
    fromConstructor _ = return ()
instance ToLunaObject () where
    toConstructor _ _ = Constructor "None" []

type instance RuntimeRepOf Bool = AsClass "Bool" Bool
instance FromLunaObject Bool where
    fromConstructor c = return $ c ^. tag == "True"
instance ToLunaObject Bool where
    toConstructor _ b = Constructor (if b then "True" else "False") []

type instance RuntimeRepOf (a, b) = AsClass "Tuple2" (a, b)
instance (FromLunaData a, FromLunaData b) => FromLunaObject (a, b) where
    fromConstructor (Constructor "Tuple2" [a, b]) = (,) <$> fromLunaData a <*> fromLunaData b
    fromConstructor (Constructor "Tuple2" _)      = throw "Malformed Tuple2 object."
    fromConstructor (Constructor n        _)      = throw $ "Expected a Tuple2 object, got " <> convert n
instance (ToLunaData a, ToLunaData b) => ToLunaObject (a, b) where
    toConstructor imps (a, b) = Constructor "Tuple2" [toLunaData imps a, toLunaData imps b]

type instance RuntimeRepOf (a, b, c) = AsClass "Tuple3" (a, b, c)
instance (FromLunaData a, FromLunaData b, FromLunaData c) => FromLunaObject (a, b, c) where
    fromConstructor (Constructor "Tuple3" [a, b, c]) = (,,) <$> fromLunaData a <*> fromLunaData b <*> fromLunaData c
    fromConstructor (Constructor "Tuple3" _)         = throw "Malformed Tuple3 object."
    fromConstructor (Constructor n        _)         = throw $ "Expected a Tuple3 object, got " <> convert n
instance (ToLunaData a, ToLunaData b, ToLunaData c) => ToLunaObject (a, b, c) where
    toConstructor imps (a, b, c) = Constructor "Tuple3" [toLunaData imps a, toLunaData imps b, toLunaData imps c]

type instance RuntimeRepOf (a, b, c, d) = AsClass "Tuple4" (a, b, c, d)
instance (FromLunaData a, FromLunaData b, FromLunaData c, FromLunaData d) => FromLunaObject (a, b, c, d) where
    fromConstructor (Constructor "Tuple4" [a, b, c, d]) = (,,,) <$> fromLunaData a <*> fromLunaData b <*> fromLunaData c <*> fromLunaData d
    fromConstructor (Constructor "Tuple4" _)            = throw "Malformed Tuple4 object."
    fromConstructor (Constructor n        _)            = throw $ "Expected a Tuple4 object, got " <> convert n
instance (ToLunaData a, ToLunaData b, ToLunaData c, ToLunaData d) => ToLunaObject (a, b, c, d) where
    toConstructor imps (a, b, c, d) = Constructor "Tuple4" [toLunaData imps a, toLunaData imps b, toLunaData imps c, toLunaData imps d]

type instance RuntimeRepOf (Maybe a) = AsClass "Maybe" (Maybe a)
instance FromLunaData a => FromLunaObject (Maybe a) where
    fromConstructor (Constructor "Nothing" [])  = return Nothing
    fromConstructor (Constructor "Just"    [a]) = Just <$> fromLunaData a
    fromConstructor _ = throw "Malformed Maybe object."
instance ToLunaData a => ToLunaObject (Maybe a) where
    toConstructor imps (Just a) = Constructor "Just" [toLunaData imps a]
    toConstructor imps Nothing  = Constructor "Nothing" []

type instance RuntimeRepOf (Either a b) = AsClass "Either" (Either a b)
instance (FromLunaData a, FromLunaData b) => FromLunaObject (Either a b) where
    fromConstructor (Constructor "Left"  [a]) = Left <$> fromLunaData a
    fromConstructor (Constructor "Right" [b]) = Left <$> fromLunaData b
    fromConstructor _ = throw "Malformed Either object."
instance (ToLunaData a, ToLunaData b) => ToLunaObject (Either a b) where
    toConstructor imps (Left  a) = Constructor "Left"  [toLunaData imps a]
    toConstructor imps (Right b) = Constructor "Right" [toLunaData imps b]

type instance RuntimeRepOf [a] = AsClass "List" [a]
instance FromLunaData a => FromLunaObject [a] where
    fromConstructor (Constructor "Empty"   [])     = return []
    fromConstructor (Constructor "Prepend" [a, b]) = (:) <$> fromLunaData a <*> fromLunaData b
    fromConstructor _ = throw "Malformed List object."
instance ToLunaData a => ToLunaObject [a] where
    toConstructor _    []       = Constructor "Empty" []
    toConstructor imps (a : as) = Constructor "Prepend" [toLunaData imps a, toLunaData imps as]


mkPrimFun :: (LunaData -> LunaValue) -> LunaData
mkPrimFun f = LunaFunction $ (>>= thunkProof f)

thunkProof :: (LunaData -> LunaValue) -> LunaData -> LunaValue
thunkProof f (LunaThunk a) = return $ LunaThunk $ a >>= thunkProof f
thunkProof f (LunaSusp  a) = return $ LunaSusp  $ a >>= thunkProof f
thunkProof f a             = f a

rethrowExceptions :: IO a -> LunaEff a
rethrowExceptions a = do
    res <- performIO $ Exception.catchAny (Right <$> a) (return . Left . displayException)
    case res of
        Left a  -> throw a
        Right r -> return r


instance {-# OVERLAPPABLE #-} (FromLunaData a, ToLunaValue b) => ToLunaData (a -> b) where
    toLunaData imps f = mkPrimFun $ \d -> fromLunaData d >>= toLunaValue imps . f

instance {-# OVERLAPPABLE #-} (FromLunaData a, ToLunaValue b) => ToLunaData (LunaEff a -> b) where
    toLunaData imps f = LunaFunction fun where
        fun v = toLunaValue imps $ f (v >>= fromLunaData)

instance ToLunaValue a => ToLunaValue (IO a) where
    toLunaValue imps = toLunaValue imps <=< rethrowExceptions

instance ToLunaData a => ToLunaValue (LunaEff a) where
    toLunaValue imps a = toLunaData imps <$> a

--------------------------------------
-- === Readable representations === --
--------------------------------------

data SingleRep = ErrorRep Text     | SuccessRep Text (Maybe Text)
data ValueRep  = OneTime SingleRep | Streaming ((SingleRep -> IO ()) -> IO ())

-- this is like `toLunaValue` but this particular case does not need imports to be present. The return value is invalid from Luna standpoint and should never be used.
feedActionToLuna :: (LunaData -> LunaEff ()) -> LunaValue
feedActionToLuna act = return $ LunaFunction $ \val -> do
    val >>= act
    return $ LunaObject $ Object (Constructor "None" []) def

listenStream :: LunaData -> (SingleRep -> IO ()) -> IO ()
listenStream val listener = do
    listenerSuccess <- runIO $ runError $ do
        each <- tryDispatchMethodWithError "each" val
        case each of
            Just e  -> void $ applyFun e $ feedActionToLuna (makeReps >=> performIO . listener)
            Nothing -> return ()
    case listenerSuccess of
        Left err -> listener $ ErrorRep $ convert err
        Right _  -> return ()

makeReps :: LunaData -> LunaEff SingleRep
makeReps val = do
    short' <- tryDispatchMethodWithError "shortRep"           val
    short  <- maybe (return "") (>>= fromLunaData) short'
    long'  <- tryDispatchMethods         ["toJSON", "render"] val
    long   <- mapM (>>= fromLunaData) long'
    return $ SuccessRep short long

getReps' :: LunaData -> LunaEff ValueRep
getReps' val = do
    forced       <- force' val
    shouldStream <- tryDispatchMethodWithError "isStream" forced >>= mapM (>>= fromLunaData)
    case shouldStream of
        Just True -> return $ Streaming $ listenStream forced
        _         -> OneTime <$> makeReps forced

getReps :: LunaData -> IO ValueRep
getReps val = do
    result <- runIO $ runError $ getReps' val
    case result of
        Left err  -> return $ OneTime $ ErrorRep $ convert err
        Right res -> return res
