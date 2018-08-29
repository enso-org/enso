{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Runtime.Data where

import Prologue hiding (Data, force)

import qualified Data.Map                  as Map
import qualified Luna.IR                   as IR
import qualified Luna.Runtime.Eff          as Luna
import qualified Control.Concurrent.Future as Future

import Data.Map                  (Map)
import GHC.Exts                  (Any)
import Control.Concurrent.Future (Future)


type DefMap = Map IR.Name (Future Value)

data Constructor = Constructor { _tag    :: IR.Name
                               , _fields :: [Data]
                               }

data Object a = Object { _unitName    :: IR.Qualified
                       , _className   :: IR.Name
                       , _constructor :: a
                       , _methods     :: DefMap
                       }

data Data = Native   (Object Any)
          | Cons     (Object Constructor)
          | Function (Value -> Value)
          | Susp     Value
          | Thunk    Value
          | Error    Text

type Value = Luna.Eff Data

makeLenses ''Object
makeLenses ''Constructor

dispatchMethod :: IR.Name -> Data -> Value
dispatchMethod s = go where
    go :: Data -> Value
    go   (Susp     a) = a >>= dispatchMethod s
    go   (Thunk    a) = a >>= dispatchMethod s
    go x@(Native   a) = dispatchObject x a
    go x@(Cons     a) = dispatchObject x a
    go   (Error    e) = Luna.throw e
    go   (Function _) = Luna.throw $ "Cannot call method: "
                                   <> convertVia @String s
                                   <> " on a function."

    dispatchObject :: Data -> Object a -> Value
    dispatchObject x (Object m c _ ms) = case Map.lookup s ms of
        Nothing -> Luna.throw $ "Cannot find method "
                              <> convertVia @String s
                              <> " of "
                              <> convertVia @IR.Name m
                              <> "."
                              <> convert c
                              <> "."
        -- | The use of `Future.unsafeGet` here is safe.
        --   The action inside future is suspended and the result cached
        --   inside an MVar, and the action itself is self–sufficient, not
        --   interacting with any outside resources.
        Just f  -> applyFun (Future.unsafeGet f) $ return x

tryDispatchMethod :: IR.Name -> Data -> Luna.Eff (Maybe Value)
tryDispatchMethod s = force' >=> go where
    go (Error e)     = Luna.throw e
    go x@(Native a)  = return $ tryDispatch x a
    go x@(Cons   a)  = return $ tryDispatch x a
    go _             = return Nothing

    tryDispatch :: Data -> Object a -> Maybe Value
    tryDispatch x (Object _ _ _ ms) = case Map.lookup s ms of
        Just f -> Just $ Future.get f >>= \fun -> applyFun fun (return x)
        _      -> Nothing

tryDispatchMethods :: [IR.Name] -> Data -> Luna.Eff (Maybe Value)
tryDispatchMethods [] s = return $ Just $ return s
tryDispatchMethods (m : ms) s = do
    res <- tryDispatchMethod m s
    case res of
        Nothing -> return Nothing
        Just r  -> r >>= tryDispatchMethods ms

applyFun :: Value -> Value -> Value
applyFun f a = do
    fun  <- f
    case fun of
        Error    e  -> Luna.throw e
        Susp     t  -> applyFun t a
        Thunk    t  -> applyFun t a
        Function f' -> f' a
        Native   _  -> Luna.throw "Object is not a function."
        Cons     _  -> Luna.throw "Object is not a function."

force :: Value -> Value
force = (>>= force')
{-# INLINE force #-}

force' :: Data -> Value
force' (Thunk a) = force a
force' (Susp  a) = force a
force' x = pure x

mkThunk :: Value -> Data
mkThunk = Thunk . force
{-# INLINE mkThunk #-}

mkSusp :: Value -> Data
mkSusp = Susp . force
{-# INLINE mkSusp #-}

forceThunks :: Value -> Value
forceThunks = (>>= forceThunks')
{-# INLINE forceThunks #-}

forceThunks' :: Data -> Value
forceThunks' (Thunk a) = forceThunks a
forceThunks' x = pure x

