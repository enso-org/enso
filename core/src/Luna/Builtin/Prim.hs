{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Luna.Builtin.Prim where

import Prologue                    hiding (force, Text)

import Control.Concurrent
import Control.Concurrent.MVar
import Data.ByteString.Lazy        hiding (head)
import Data.Maybe                  (fromMaybe, maybeToList)
import Data.Text.Lazy              hiding (head)
import GHC.Exts                    (Any)
import Luna.Builtin.Data.LunaEff   (LunaEff, performIO, runError, runIO, throw)
import Luna.Builtin.Data.LunaValue
import Luna.Builtin.Data.Module
import Luna.Builtin.Data.Stream    (Stream)
import GHC.TypeLits
import Data.Proxy
import Unsafe.Coerce

class KnownSymbol n => IsBoxed n a | a -> n, n -> a where
    toBoxed :: Imports -> a -> Object Any
    toBoxed imps i = Object (unsafeCoerce i) $ getObjectMethodMap (convert $ symbolVal $ Proxy @n) imps

    fromBoxed :: Object Any -> a
    fromBoxed (Object s _) = unsafeCoerce s

instance IsBoxed "Int"    Int
instance IsBoxed "Double" Double
instance IsBoxed "Text"   Text
instance IsBoxed "Binary" ByteString
instance IsBoxed "MVar"   (MVar LunaData)

class FromLunaData a where
    fromLunaData :: LunaData -> LunaEff a

instance {-# OVERLAPPABLE #-} IsBoxed n a => FromLunaData a where
    fromLunaData (LunaBoxed a)    = return $ fromBoxed a
    fromLunaData (LunaThunk a)    = a >>= fromLunaData
    fromLunaData (LunaObject _)   = throw "Expected a Boxed value, got an Object"
    fromLunaData (LunaFunction _) = throw "Expected a Boxed value, got a Function"
    fromLunaData (LunaSusp _)     = throw "Expected a Boxed value, got a Suspension"
    fromLunaData LunaNoValue      = throw "Expected a Boxed value, got a Compilation Error"
    fromLunaData (LunaError e)    = throw e

instance FromLunaData LunaData where
    fromLunaData = return

class ToLunaData a where
    toLunaData :: Imports -> a -> LunaData

instance {-# OVERLAPPABLE #-} IsBoxed n a => ToLunaData a where
    toLunaData = LunaBoxed .: toBoxed

instance ToLunaData LunaData where
    toLunaData _ = id

instance ToLunaData Bool where
    toLunaData imps b = LunaObject $ Object (Constructor (if b then "True" else "False") []) $ getObjectMethodMap "Bool" imps

instance {-# OVERLAPPABLE #-} ToLunaData a => ToLunaData [a] where
    toLunaData imps []       = LunaObject $ Object (Constructor "Empty"   [])                                      $ getObjectMethodMap "List" imps
    toLunaData imps (a : as) = LunaObject $ Object (Constructor "Prepend" [toLunaData imps a, toLunaData imps as]) $ getObjectMethodMap "List" imps

instance ToLunaData Text where
    toLunaData = LunaBoxed .: toBoxed

instance (ToLunaData a, ToLunaData b, ToLunaData c) => ToLunaData (a, b, c) where
    toLunaData imps (a, b, c) = LunaObject $ Object (Constructor "Triple" [toLunaData imps a, toLunaData imps b, toLunaData imps c]) $ getObjectMethodMap "Triple" imps

instance ToLunaData a => ToLunaData (Maybe a) where
    toLunaData imps Nothing  = LunaObject $ Object (Constructor "Nothing" [])               $ getObjectMethodMap "Maybe" imps
    toLunaData imps (Just a) = LunaObject $ Object (Constructor "Just" [toLunaData imps a]) $ getObjectMethodMap "Maybe" imps


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

class ToLunaValue a where
    toLunaValue :: Imports -> a -> LunaValue

instance {-# OVERLAPPABLE #-} ToLunaData a => ToLunaValue a where
    toLunaValue = return .: toLunaData

instance {-# OVERLAPPABLE #-} (FromLunaData a, ToLunaValue b) => ToLunaData (a -> b) where
    toLunaData imps f = mkPrimFun $ \d -> fromLunaData d >>= toLunaValue imps . f

instance {-# OVERLAPPABLE #-} (FromLunaData a, ToLunaValue b) => ToLunaData (LunaEff a -> b) where
    toLunaData imps f = LunaFunction fun where
        fun v = toLunaValue imps $ f (v >>= fromLunaData)

instance ToLunaData a => ToLunaValue (LunaEff a) where
    toLunaValue imps a = toLunaData imps <$> a

mkPrimFun :: (LunaData -> LunaValue) -> LunaData
mkPrimFun f = LunaFunction $ (>>= thunkProof f)

thunkProof :: (LunaData -> LunaValue) -> LunaData -> LunaValue
thunkProof f (LunaThunk a) = return $ LunaThunk $ a >>= thunkProof f
thunkProof f a             = f a


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
