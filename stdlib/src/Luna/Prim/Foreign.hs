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
import           Foreign.C.Types             (CDouble(..), CFloat(..))
import qualified Foreign.LibFFI              as LibFFI
import           Foreign.Marshal.Alloc       (alloca, allocaBytes)
import           Foreign.Ptr                 (Ptr, FunPtr, castPtr, castPtrToFunPtr, nullPtr, plusPtr)
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
        primLookupSymbolVal (convert -> dll) (convert -> symbol) = do
            dl       <- Linker.dlopen dll [Linker.RTLD_LAZY]
            sym      <- Linker.dlsym dl symbol
            return $ Just sym
    primLookupSymbol <- makeFunctionIO (toLunaValue std primLookupSymbolVal) ["Text", "Text"] $ LCons "Maybe" ["FunPtr"]

    let primCallFunPtrVal :: FunPtr LunaData -> ExistRetType -> [LibFFI.Arg] -> IO LunaData
        primCallFunPtrVal funPtr retType args = case retType of
            ExistRetType c -> toLunaData std <$> LibFFI.callFFI funPtr c args
    primCallFunPtr <- makeFunctionIO (toLunaValue std primCallFunPtrVal) ["FunPtr", "c", LCons "List" ["a"]] "b"

    primNullPtr <- makeFunctionPure (toLunaValue std (nullPtr :: Ptr LunaData)) [] "Ptr"

    let primPtrToCArgVal :: Ptr LunaData -> LibFFI.Arg
        primPtrToCArgVal ptr = LibFFI.argPtr ptr
    primPtrToCArg <- makeFunctionPure (toLunaValue std primPtrToCArgVal) ["Ptr"] "Arg"

    let primReadPtrIntVal :: Ptr LunaData -> IO Integer
        primReadPtrIntVal ptr = (fromIntegral :: Int64 -> Integer) <$> (peek (castPtr ptr) :: IO Int64)
    primReadPtrInt <- makeFunctionIO (toLunaValue std primReadPtrIntVal) ["Ptr"] "Int"

    let primWritePtrIntVal :: Ptr LunaData -> Integer -> IO ()
        primWritePtrIntVal ptr i = poke (castPtr ptr) (fromIntegral i :: Int64)
    primWritePtrInt <- makeFunctionIO (toLunaValue std primWritePtrIntVal) ["Ptr", "Int"] "None"

    let primReadPtrDoubleVal :: Ptr LunaData -> IO Double
        primReadPtrDoubleVal ptr = (\(CDouble d) -> d) <$> (peek (castPtr ptr) :: IO CDouble)
    primReadPtrDouble <- makeFunctionIO (toLunaValue std primReadPtrDoubleVal) ["Ptr"] "Real"

    let primWritePtrDoubleVal :: Ptr LunaData -> Double -> IO ()
        primWritePtrDoubleVal ptr d = poke (castPtr ptr) (coerce d :: CDouble)
    primWritePtrDouble <- makeFunctionIO (toLunaValue std primWritePtrDoubleVal) ["Ptr", "Real"] "None"

    let primReadPtrFloatVal :: Ptr LunaData -> IO Double
        primReadPtrFloatVal ptr = (\(CFloat d) -> realToFrac d) <$> (peek (castPtr ptr) :: IO CFloat)
    primReadPtrFloat <- makeFunctionIO (toLunaValue std primReadPtrFloatVal) ["Ptr"] "Real"

    let primWritePtrFloatVal :: Ptr LunaData -> Double -> IO ()
        primWritePtrFloatVal ptr d = poke (castPtr ptr) (realToFrac d :: CFloat)
    primWritePtrFloat <- makeFunctionIO (toLunaValue std primWritePtrFloatVal) ["Ptr", "Real"] "None"

    let primPlusPtrVal :: Ptr LunaData -> Integer -> Ptr LunaData
        primPlusPtrVal ptr bytes = ptr `plusPtr` fromIntegral bytes
    primPlusPtr <- makeFunctionPure (toLunaValue std primPlusPtrVal) ["Ptr", "Int"] "Ptr"

    let primByteSizeFloatVal :: Integer
        primByteSizeFloatVal = fromIntegral $ sizeOf (undefined :: CFloat)
    primByteSizeFloat <- makeFunctionPure (toLunaValue std primByteSizeFloatVal) [] "Int"
    let primIntToCSizeVal :: Integer -> LibFFI.Arg
        primIntToCSizeVal i = LibFFI.argCSize $ fromIntegral i
    primIntToCSize <- makeFunctionPure (toLunaValue std primIntToCSizeVal) ["Int"] "Arg"
    return $ Map.fromList [ ("primLookupSymbol", primLookupSymbol)
                          , ("primCallFunPtr", primCallFunPtr)
                          , ("primNullPtr", primNullPtr)
                          , ("primPtrToCArg", primPtrToCArg)
                          , ("primIntToCSize", primIntToCSize)
                          , ("primPlusPtr", primPlusPtr)
                          , ("primReadPtrInt", primReadPtrInt)
                          , ("primWritePtrInt", primWritePtrInt)
                          , ("primReadPtrDouble", primReadPtrDouble)
                          , ("primWritePtrDouble", primWritePtrDouble)
                          , ("primReadPtrFloat", primReadPtrFloat)
                          , ("primWritePtrFloat", primWritePtrFloat)
                          , ("primByteSizeFloat", primByteSizeFloat)
                          ]

data ExistRetType = forall a. ToLunaData a => ExistRetType (LibFFI.RetType a)
instance FromLunaData ExistRetType where
    fromLunaData t = force' t >>= \case
        LunaObject obj -> case obj ^. constructor . tag of
            "CVoid"      -> return $ ExistRetType   LibFFI.retVoid
            "CInt"       -> return $ ExistRetType $ (fmap fromIntegral LibFFI.retCInt   :: LibFFI.RetType Integer)
            "CInt8"      -> return $ ExistRetType $ (fmap fromIntegral LibFFI.retInt8   :: LibFFI.RetType Integer)
            "CInt16"     -> return $ ExistRetType $ (fmap fromIntegral LibFFI.retInt16  :: LibFFI.RetType Integer)
            "CInt32"     -> return $ ExistRetType $ (fmap fromIntegral LibFFI.retInt32  :: LibFFI.RetType Integer)
            "CInt64"     -> return $ ExistRetType $ (fmap fromIntegral LibFFI.retInt64  :: LibFFI.RetType Integer)
            "CUInt"      -> return $ ExistRetType $ (fmap fromIntegral LibFFI.retCUInt  :: LibFFI.RetType Integer)
            "CUInt8"     -> return $ ExistRetType $ (fmap fromIntegral LibFFI.retWord8  :: LibFFI.RetType Integer)
            "CUInt16"    -> return $ ExistRetType $ (fmap fromIntegral LibFFI.retWord16 :: LibFFI.RetType Integer)
            "CUInt32"    -> return $ ExistRetType $ (fmap fromIntegral LibFFI.retWord32 :: LibFFI.RetType Integer)
            "CUInt64"    -> return $ ExistRetType $ (fmap fromIntegral LibFFI.retWord64 :: LibFFI.RetType Integer)
            "CLong"      -> return $ ExistRetType $ (fmap fromIntegral LibFFI.retCLong  :: LibFFI.RetType Integer)
            "CLongLong"  -> return $ ExistRetType $ (fmap fromIntegral LibFFI.retInt64  :: LibFFI.RetType Integer)
            "CULongLong" -> return $ ExistRetType $ (fmap fromIntegral LibFFI.retWord64 :: LibFFI.RetType Integer)
            "CSize"      -> return $ ExistRetType $ (fmap fromIntegral LibFFI.retCSize  :: LibFFI.RetType Integer)
            "CFloat"     -> return $ ExistRetType $ fmap (\d -> realToFrac d :: Double) LibFFI.retCFloat
            "CDouble"    -> return $ ExistRetType $ fmap (\(CDouble d) -> d) LibFFI.retCDouble
            "Ptr"        -> return $ ExistRetType $ LibFFI.retPtr (undefined :: LibFFI.RetType LunaData)
            "CString"    -> return $ ExistRetType $ fmap Text.pack LibFFI.retString
            invalid      -> throw $ "CFFI: invalid return type: " <> convert invalid

type instance RuntimeRepOf (FunPtr LunaData)      = AsNative "FunPtr"
type instance RuntimeRepOf (Ptr LunaData)         = AsNative "Ptr"
