{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Luna.Prim.Foreign where

import qualified Prelude                     as P
import           Luna.Prelude                hiding (Text, throwM)
import           Luna.IR

import           Control.Exception.Safe      (handleAny, throwString)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text.Lazy              (Text)
import           Foreign.C.Types             (CDouble(..))
import qualified Foreign.LibFFI              as LibFFI
import           Foreign.Ptr                 (Ptr, FunPtr, castPtrToFunPtr, nullPtr)
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

    return $ Map.fromList [ ("primLookupSymbol", primLookupSymbol)
                          , ("primCallFunPtr", primCallFunPtr)
                          , ("primNullPtr", primNullPtr)
                          , ("primPtrToCArg", primPtrToCArg)
                          ]

data ExistRetType = forall a. ToLunaData a => ExistRetType (LibFFI.RetType a)

instance FromLunaData ExistRetType where
    fromLunaData t = force' t >>= \case
        LunaObject obj -> case obj ^. constructor . tag of
            "CInt"     -> return $ ExistRetType $ (fmap fromIntegral LibFFI.retInt :: LibFFI.RetType Integer)
            "CVoid"    -> return $ ExistRetType LibFFI.retVoid
            "CDouble"  -> return $ ExistRetType $ fmap (\(CDouble d) -> d) LibFFI.retCDouble
            "Ptr"      -> return $ ExistRetType $ LibFFI.retPtr (undefined :: LibFFI.RetType LunaData)
            _          -> throw "unknown return type"

type instance RuntimeRepOf (FunPtr LunaData)      = AsNative "FunPtr"
type instance RuntimeRepOf (Ptr LunaData)         = AsNative "Ptr"
