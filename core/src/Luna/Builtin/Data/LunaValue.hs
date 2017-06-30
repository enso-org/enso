{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Luna.Builtin.Data.LunaValue where

import Luna.Prelude   as P hiding (seq, force, Constructor, Text)
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import Luna.Builtin.Data.LunaEff    (runError, throw, runIO, performIO, LunaEff)
import Data.Text.Lazy

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio (numerator)
import Data.Maybe (fromMaybe, maybeToList)
import Luna.IR

data Constructor = Constructor { _tag    :: Name
                               , _fields :: [LunaData]
                               }

data Object a = Object { _constructor :: a
                       , _methods     :: Map Name LunaValue
                       }

data LunaData = LunaBoxed    (Object Any)
              | LunaObject   (Object Constructor)
              | LunaFunction (LunaValue -> LunaValue)
              | LunaSusp     LunaValue
              | LunaThunk    LunaValue
              | LunaError    P.String
              | LunaNoValue

type LunaValue = LunaEff LunaData

makeLenses ''Object
makeLenses ''Constructor

dispatchMethod :: Name -> LunaData -> LunaValue
dispatchMethod s = go where
    go :: LunaData -> LunaValue
    go   (LunaThunk    a) = a >>= dispatchMethod s
    go x@(LunaBoxed    a) = dispatchObject x a
    go x@(LunaObject   a) = dispatchObject x a
    go   (LunaError    e) = throw e
    go   (LunaFunction _) = throw $ "Cannot call method: " ++ show s ++ " on a function."
    go   LunaNoValue      = return LunaNoValue

    dispatchObject :: LunaData -> Object a -> LunaValue
    dispatchObject x (Object _ ms) = applyFun (fromMaybe (throw $ "Cannot find method: " ++ show s ++ ".") $ Map.lookup s ms) $ return x

tryDispatchMethodWithError :: Name -> LunaData -> LunaEff (Maybe LunaValue)
tryDispatchMethodWithError s = go where
    go   (LunaError  e) = throw e
    go   (LunaThunk  a) = a >>= tryDispatchMethodWithError s
    go x@(LunaBoxed  a) = return $ tryDispatch x a
    go x@(LunaObject a) = return $ tryDispatch x a
    go _                = return Nothing

    tryDispatch :: LunaData -> Object a -> Maybe LunaValue
    tryDispatch x (Object _ ms) = (flip applyFun $ return x) <$> Map.lookup s ms

tryDispatchMethods :: [Name] -> LunaData -> LunaEff (Maybe LunaValue)
tryDispatchMethods [] s = return $ Just $ return s
tryDispatchMethods (m : ms) s = do
    res <- tryDispatchMethodWithError m s
    case res of
        Nothing -> return Nothing
        Just r  -> r >>= tryDispatchMethods ms

force' :: LunaData -> LunaValue
force' (LunaThunk a) = force a
force' a             = return a

force :: LunaValue -> LunaValue
force = (>>= force')

applyFun :: LunaValue -> LunaValue -> LunaValue
applyFun f a = do
    fun  <- f
    case fun of
        LunaError    e  -> throw e
        LunaThunk    t  -> applyFun t a
        LunaFunction f' -> f' a
        LunaBoxed    _  -> throw "Object is not a function."
        LunaObject   _  -> throw "Object is not a function."
        LunaNoValue     -> return LunaNoValue

