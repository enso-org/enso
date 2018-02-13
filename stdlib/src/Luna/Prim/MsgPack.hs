{-# LANGUAGE OverloadedStrings #-}

module Luna.Prim.MsgPack where

import qualified Prelude                     as P
import           Luna.Prelude                hiding (Text)
import           Luna.IR

import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy        as ByteString
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import qualified Data.MessagePack            as MsgPack
import           Data.Text.Lazy              (Text)
import qualified Data.Text.Lazy              as Text

import           Luna.Builtin.Prim           (ToLunaValue, toLunaValue, ToLunaData, toLunaData, FromLunaData, fromLunaData)
import           Luna.Builtin.Data.Function  (Function)
import           Luna.Builtin.Data.Module    (Imports, getObjectMethodMap)
import           Luna.Builtin.Data.LunaValue (LunaData (..), Constructor (..), Object (..), constructor, tag, fields, force')
import           Luna.Builtin.Data.LunaEff   (throw)
import           Luna.Std.Builder            (makeFunctionIO, makeFunctionPure, LTp (..))

exports :: Imports -> IO (Map Name Function)
exports std = do
    let encodeMsgPackVal = toLunaValue std (MsgPack.pack :: MsgPack.Object -> ByteString)
    encodeMsgPack <- makeFunctionPure encodeMsgPackVal ["MsgPack"] "Binary"

    let parseMsgPackVal :: ByteString -> IO MsgPack.Object
        parseMsgPackVal = MsgPack.unpack
    parseMsgPack <- makeFunctionIO (toLunaValue std parseMsgPackVal) ["Binary"] $ LCons "MsgPack" []
    return $ Map.fromList [ ("parseMsgPack", parseMsgPack)
                          , ("encodeMsgPack", encodeMsgPack)
                          ]

unexpectedConstructorFor name = throw $ "Expected a " <> name <> " luna object, got unexpected constructor"

instance FromLunaData MsgPack.Object where
    fromLunaData v = force' v >>= \case
        LunaObject obj -> case obj ^. constructor . tag of
            "MPNull"   -> return MsgPack.ObjectNil
            "MPBool"   -> MsgPack.ObjectBool <$> (fromLunaData . head $ obj ^. constructor . fields)
            "MPInt"    -> (MsgPack.ObjectInt . (fromIntegral :: Integer -> Int64)) <$> (fromLunaData . head $ obj ^. constructor . fields)
            "MPReal"   -> MsgPack.ObjectDouble <$> (fromLunaData . head $ obj ^. constructor . fields)
            "MPString" -> (MsgPack.ObjectStr . Text.toStrict)       <$> (fromLunaData . head $ obj ^. constructor . fields)
            "MPBinary" -> (MsgPack.ObjectBin . ByteString.toStrict) <$> (fromLunaData . head $ obj ^. constructor . fields)
            "MPArray"  -> MsgPack.ObjectArray <$> (fromLunaData . head $ obj ^. constructor . fields)
            "MPMap"    -> MsgPack.ObjectMap   <$> (fromLunaData . head $ obj ^. constructor . fields)
            _          -> unexpectedConstructorFor "MsgPack"
        _ -> unexpectedConstructorFor "MsgPack"

instance ToLunaData MsgPack.Object where
    toLunaData imps  MsgPack.ObjectNil       = LunaObject $ Object (Constructor "MPNull"   [])                                              (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectBool   b) = LunaObject $ Object (Constructor "MPBool"   [toLunaData imps b])                             (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectInt    i) = LunaObject $ Object (Constructor "MPInt"    [toLunaData imps $ (fromIntegral i :: Integer)]) (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectWord   w) = LunaObject $ Object (Constructor "MPInt"    [toLunaData imps $ (fromIntegral w :: Integer)]) (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectFloat  f) = LunaObject $ Object (Constructor "MPReal"   [toLunaData imps $ (realToFrac   f :: Double)])  (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectDouble d) = LunaObject $ Object (Constructor "MPReal"   [toLunaData imps d])                             (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectStr    s) = LunaObject $ Object (Constructor "MPString" [toLunaData imps $ Text.fromStrict s])           (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectBin    b) = LunaObject $ Object (Constructor "MPBinary" [toLunaData imps $ ByteString.fromStrict b])     (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectArray  l) = LunaObject $ Object (Constructor "MPArray"  [toLunaData imps l])                             (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectMap    m) = LunaObject $ Object (Constructor "MPMap"    [toLunaData imps m])                             (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectExt  _ _) = LunaError "MessagePack ObjectExt is not supported."

