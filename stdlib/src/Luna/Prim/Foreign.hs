{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Luna.Prim.Foreign where

import qualified Prelude                     as P
import           Luna.Prelude                hiding (Text, throwM)
import           Luna.IR

import           Control.Exception.Safe      (handleAny, throwString)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text.Lazy              (Text)
import qualified Data.Text.Lazy              as Text
import           Foreign.C.Types             (CDouble(..))
import qualified Foreign.LibFFI              as LibFFI
import           Foreign.Marshal.Alloc       (alloca, allocaBytes)
import           Foreign.Ptr                 (Ptr, FunPtr, castPtr, castPtrToFunPtr, nullPtr)
import           Foreign.Storable            (Storable(..))
import qualified System.Posix.DynamicLinker  as Linker
import           Luna.Builtin.Data.Function  (Function)
import           Luna.Builtin.Data.Module    (Imports, getObjectMethodMap)
import           Luna.Builtin.Data.LunaValue (LunaData (LunaObject), Constructor (..), Object (..), constructor, tag, fields, force')
import           Luna.Builtin.Data.LunaEff   (LunaEff, throw, runIO, runError)
import           Luna.Builtin.Prim           (RuntimeRep(..), RuntimeRepOf, toLunaValue, ToLunaData, toLunaData, FromLunaData, fromLunaData)
import           Luna.Std.Builder            (makeFunctionIO, makeFunctionPure, maybeLT, LTp (..), int, integer, real)


exports :: Imports -> IO (Map Name Function)
exports std = do
    let primLookupSymbolVal :: Text -> Text -> IO (Maybe (FunPtr LunaData))
        primLookupSymbolVal (convert -> dll) (convert -> symbol) = handleAny (\_ -> return Nothing) $ do
            dl <- Linker.dlopen dll [Linker.RTLD_LAZY]
            Just <$> Linker.dlsym dl symbol
    primLookupSymbol <- makeFunctionIO (toLunaValue std primLookupSymbolVal) ["Text", "Text"] $ LCons "Maybe" ["FunPtr"]

    let primCallFunPtrVal :: FunPtr LunaData -> ExistRetType -> [LibFFI.Arg] -> IO LunaData
        primCallFunPtrVal funPtr retType args = case retType of
            ExistRetType c -> toLunaData std <$> LibFFI.callFFI funPtr c args
    primCallFunPtr <- makeFunctionIO (toLunaValue std primCallFunPtrVal) ["FunPtr", "c", LCons "List" ["a"]] "b"

    primNullPtr <- makeFunctionPure (toLunaValue std (nullPtr :: Ptr LunaData)) [] "Ptr"

    let primPtrToCArgVal :: Ptr LunaData -> LibFFI.Arg
        primPtrToCArgVal ptr = LibFFI.argPtr ptr
    primPtrToCArg <- makeFunctionPure (toLunaValue std primPtrToCArgVal) ["Ptr"] "Arg"

    let primPeekPtrVal :: Ptr LunaData -> ExistStorable -> IO LunaData
        primPeekPtrVal ptr exist = genericPeek exist ptr (toLunaData std)
    primPeekPtr <- makeFunctionPure (toLunaValue std primPeekPtrVal) ["Ptr", "b"] "a"

    let primPokePtrVal :: Ptr LunaData -> LunaData -> ExistStorable -> IO (Ptr LunaData)
        primPokePtrVal ptr val exist = genericPoke exist ptr val >> return ptr
    primPokePtr <- makeFunctionPure (toLunaValue std primPokePtrVal) ["Ptr", "a", "b"] "Ptr"

    return $ Map.fromList [ ("primLookupSymbol", primLookupSymbol)
                          , ("primCallFunPtr", primCallFunPtr)
                          , ("primNullPtr", primNullPtr)
                          , ("primPtrToCArg", primPtrToCArg)
                          , ("primPeekPtr", primPeekPtr)
                          , ("primPokePtr", primPokePtr)
                          ]

data ExistRetType = forall a. ToLunaData a => ExistRetType (LibFFI.RetType a)
data StorableDict a where
    StorableDict :: Storable a => StorableDict a

genericPeek :: ExistStorable -> Ptr LunaData -> (forall a. ToLunaData a => a -> LunaData) -> IO LunaData
genericPeek exist ptr tld = case exist of
    ExistStorable conv _ c -> case c of
        StorableDict -> (tld . conv) <$> peek (castPtr ptr `asDict` c)

genericPoke :: ExistStorable -> Ptr LunaData -> LunaData -> IO ()
genericPoke exist ptr lval = case exist of
    ExistStorable _ conv c -> case c of
        StorableDict -> do
            val <- runIO $ runError $ fromLunaData lval
            case val of
                Left err -> throwString err
                Right v  -> poke (castPtr ptr `asDict` c) (conv v)

asDict :: Ptr a -> StorableDict a -> Ptr a
asDict ptr _ = ptr

data ExistStorable = forall a b c. (ToLunaData b, FromLunaData c) => ExistStorable (a -> b) (c -> a) (StorableDict a)

instance FromLunaData ExistRetType where
    fromLunaData t = force' t >>= \case
        LunaObject obj -> case obj ^. constructor . tag of
            "CInt"     -> return $ ExistRetType $ (fmap fromIntegral LibFFI.retInt :: LibFFI.RetType Integer)
            "CVoid"    -> return $ ExistRetType LibFFI.retVoid
            "CDouble"  -> return $ ExistRetType $ fmap (\(CDouble d) -> d) LibFFI.retCDouble
            "Ptr"      -> return $ ExistRetType $ LibFFI.retPtr (undefined :: LibFFI.RetType LunaData)
            "CString"  -> return $ ExistRetType $ fmap Text.pack LibFFI.retString
            _          -> throw "unknown return type"

instance FromLunaData ExistStorable where
    fromLunaData t = force' t >>= \case
        LunaObject obj -> case obj ^. constructor . tag of
            "CInt"     -> return $ ExistStorable (fromIntegral :: Int -> Integer) (fromIntegral :: Integer -> Int) (StorableDict :: StorableDict Int)
            "CDouble"  -> return $ ExistStorable id id (StorableDict :: StorableDict Double)
            "Ptr"      -> return $ ExistStorable (\a -> castPtr a :: Ptr LunaData) (castPtr :: Ptr LunaData -> Ptr a) (StorableDict :: StorableDict (Ptr a))
            _          -> throw "unknown peek type"

type instance RuntimeRepOf (FunPtr LunaData)      = AsNative "FunPtr"
type instance RuntimeRepOf (Ptr LunaData)         = AsNative "Ptr"
