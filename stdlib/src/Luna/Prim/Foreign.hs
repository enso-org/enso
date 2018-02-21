{-# LANGUAGE OverloadedStrings #-}

module Luna.Prim.Foreign where

import qualified Prelude                     as P
import           Luna.Prelude                hiding (Text, throwM)
import           Luna.IR

import           Control.Exception.Safe      (handleAny, throwString)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text.Lazy              (Text)
import qualified Foreign.LibFFI              as LibFFI
import           Foreign.Ptr                 (FunPtr, castPtrToFunPtr)
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
    primLookupSymbol <- makeFunctionIO (toLunaValue std primLookupSymbolVal) ["Text", "Text"] $ LCons "Maybe" [LCons "FunPtr" []] 

    let primCallFunPtrVal :: FunPtr LunaData -> [LunaData] -> IO Integer
        primCallFunPtrVal funPtr args = do
            args <- runIO $ runError $ mapM fromLunaData args :: IO (Either P.String [LibFFI.Arg])
            case args of
                Left err -> throwString err
                Right a  -> fromIntegral <$> LibFFI.callFFI funPtr LibFFI.retCInt a
    primCallFunPtr <- makeFunctionIO (toLunaValue std primCallFunPtrVal) [LCons "FunPtr" [], LCons "List" [LVar "a"]] $ LCons "Int" []

    return $ Map.fromList [ ("primLookupSymbol", primLookupSymbol)
                          , ("primCallFunPtr", primCallFunPtr)
                          ]

-- instance FromLunaData Time.ZonedTime where
--     fromLunaData zt = let errorMsg = "Expected a Time Luna object, got unexpected constructor" in
--         force' zt >>= \case
--             LunaObject obj -> case obj ^. constructor . tag of
--                 "TimeVal" -> Time.ZonedTime <$> localTime <*> (fromLunaData tz)
--                                       where [days, tod, tz] = obj ^. constructor . fields
--                                             localTime       = Time.LocalTime <$> fromLunaData days <*> fromLunaData tod
--                 _              -> throw errorMsg
--             _ -> throw errorMsg

type instance RuntimeRepOf (FunPtr LunaData)      = AsNative "FunPtr"

-- instance ToLunaData Time.ZonedTime where
--     toLunaData imps (Time.ZonedTime (Time.LocalTime days tod) tz) = LunaObject $
--         Object (Constructor "TimeVal" [toLunaData imps days, toLunaData imps tod, toLunaData imps tz])
--                (getObjectMethodMap "Time" imps)
