{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module Luna.Runtime.Marshal where

import Prologue

import qualified GHC.TypeLits                as Symbol
import qualified Luna.IR                     as IR
import qualified Luna.Runtime.Data           as Luna
import qualified Luna.Runtime.Data.Evaluated as Luna
import qualified Luna.Runtime.Eff            as Luna

import GHC.Exts      (Any)
import Unsafe.Coerce (unsafeCoerce)

data ClassRep = ClassRep Symbol Symbol

data RuntimeRep = AsNative ClassRep
                | AsClass  Type ClassRep

type family GetRepClassName (a :: RuntimeRep) :: Symbol where
    GetRepClassName (AsNative   ('ClassRep _ s)) = s
    GetRepClassName (AsClass  _ ('ClassRep _ s)) = s

type family GetRepModuleName (a :: RuntimeRep) :: Symbol where
    GetRepModuleName (AsNative   ('ClassRep s _)) = s
    GetRepModuleName (AsClass  _ ('ClassRep s _)) = s

type family IsNativeRep (a :: RuntimeRep) where
    IsNativeRep (AsNative _) = True

type family IsClass (a :: RuntimeRep) where
    IsClass (AsClass _ _) = True

type family RuntimeRepOf a = (k :: RuntimeRep) | k -> a

----------------------------
-- === Native objects === --
----------------------------

type IsNative a = ( IsNativeRep (RuntimeRepOf a) ~ True
                  , KnownSymbol (GetRepClassName  (RuntimeRepOf a))
                  , KnownSymbol (GetRepModuleName (RuntimeRepOf a))
                  )

classNameOf :: forall a. (KnownSymbol (GetRepClassName (RuntimeRepOf a)))
            => IR.Name
classNameOf = convert $ Symbol.symbolVal
                  $ Proxy @(GetRepClassName (RuntimeRepOf a))

moduleNameOf :: forall a. (KnownSymbol (GetRepModuleName (RuntimeRepOf a)))
             => IR.Qualified
moduleNameOf = convertVia @IR.Name $ Symbol.symbolVal
                  $ Proxy @(GetRepModuleName (RuntimeRepOf a))

toNative :: forall a. IsNative a => Luna.Units -> a -> Luna.Object Any
toNative imps i = Luna.Object (moduleNameOf @a)
                              (classNameOf @a)
                              (unsafeCoerce i)
                              (Luna.getObjectMethodMap imps
                                                       (moduleNameOf @a)
                                                       (classNameOf @a))

fromNative :: IsNative a => Luna.Object Any -> a
fromNative (Luna.Object _ _ s _) = unsafeCoerce s

----------------------
-- === IsObject === --
----------------------

class ( KnownSymbol (GetRepClassName (RuntimeRepOf a))
      , KnownSymbol (GetRepModuleName (RuntimeRepOf a))
      , IsClass (RuntimeRepOf a) ~ True
      ) => ToObject a where
    toConstructor   :: Luna.Units -> a -> Luna.Constructor

class (IsClass (RuntimeRepOf a) ~ True) => FromObject a where
    fromConstructor :: Luna.Constructor -> Luna.Eff a

fromObject :: FromObject a => Luna.Object Luna.Constructor -> Luna.Eff a
fromObject (Luna.Object _ _ a _) = fromConstructor a

toObject ::
    forall a. ToObject a => Luna.Units -> a -> Luna.Object Luna.Constructor
toObject imps a = Luna.Object (moduleNameOf @a)
                              (classNameOf @a)
                              (toConstructor imps a)
                              (Luna.getObjectMethodMap imps
                                                       (moduleNameOf @a)
                                                       (classNameOf @a))

-------------------------------
-- === ToData / FromData === --
-------------------------------

class FromData' (k :: RuntimeRep) a where
    fromData' :: Luna.Data -> Luna.Eff a

instance (IsNative a, RuntimeRepOf a ~ AsNative n)
      => FromData' (AsNative n) a where
    fromData' = \case
        Luna.Native   a -> return $ fromNative a
        Luna.Susp     a -> a >>= fromData' @(AsNative n)
        Luna.Thunk    a -> a >>= fromData' @(AsNative n)
        Luna.Cons     _ -> Luna.throw "Expected a Native value, got an Object"
        Luna.Function _ -> Luna.throw "Expected a Native value, got a Function"
        Luna.Error    e -> Luna.throw e

instance (RuntimeRepOf a ~ AsClass a n, FromObject a)
      => FromData' (AsClass a n) a where
    fromData' = \case
        Luna.Susp     a -> a >>= fromData' @(AsClass a n)
        Luna.Thunk    a -> a >>= fromData' @(AsClass a n)
        Luna.Cons     a -> fromObject a
        Luna.Native   _ -> Luna.throw "Expected an Object, got a native value"
        Luna.Function _ -> Luna.throw "Expected an Object, got a Function"
        Luna.Error    e -> Luna.throw e

type IsData a = (FromData a, ToData a)

class FromData a where
    fromData :: Luna.Data -> Luna.Eff a

instance {-# OVERLAPPABLE #-} FromData' (RuntimeRepOf a) a => FromData a where
    fromData = fromData' @(RuntimeRepOf a)

instance FromData Luna.Data where
    fromData = return

class ToData' (k :: RuntimeRep) a where
    toData' :: Luna.Units -> a -> Luna.Data

instance (RuntimeRepOf a ~ AsNative n, IsNative a)
      => ToData' (AsNative n) a where
    toData' = Luna.Native .: toNative

instance (RuntimeRepOf a ~ AsClass a n, ToObject a)
      => ToData' (AsClass a n) a where
    toData' = Luna.Cons .: toObject

class ToData a where
    toData :: Luna.Units -> a -> Luna.Data

instance {-# OVERLAPPABLE #-} ToData' (RuntimeRepOf a) a => ToData a where
    toData = toData' @(RuntimeRepOf a)

instance ToData Luna.Data where
    toData _ = id

---------------------------------
-- === ToValue / FromValue === --
---------------------------------

class ToValue a where
    toValue :: Luna.Units -> a -> Luna.Value

instance {-# OVERLAPPABLE #-} ToData a => ToValue a where
    toValue = pure .: toData

class FromValue a where
    fromValue :: Luna.Value -> Luna.Eff a

instance {-# OVERLAPPABLE #-} FromData a => FromValue a where
    fromValue = (>>= fromData)


-----------------------
-- === Instances === --
-----------------------

handleSusps :: (Luna.Data -> Luna.Value) -> Luna.Data -> Luna.Value
handleSusps f (Luna.Susp  a) = return $ Luna.Susp $ a >>= handleSusps f
handleSusps f (Luna.Thunk a) = return $ Luna.Thunk $ a >>= handleSusps f
handleSusps f a = f a

instance {-# OVERLAPPABLE #-} (FromData a, ToValue b) => ToData (a -> b) where
    toData imps f =
        Luna.Function (>>= handleSusps (\d -> fromData d >>= toValue imps . f))

instance {-# OVERLAPPABLE #-} (FromData a, ToValue b)
      => ToData (Luna.Eff a -> b) where
    toData imps f = Luna.Function fun where
        fun v = toValue imps $ f (v >>= fromData)

instance ToValue a => ToValue (IO a) where
    toValue imps = toValue imps <=< liftIO

instance ToData a => ToValue (Luna.Eff a) where
    toValue imps a = toData imps <$> a

type instance RuntimeRepOf Integer = AsNative ('ClassRep "Std.Base" "Int")
type instance RuntimeRepOf Double  = AsNative ('ClassRep "Std.Base" "Real")
type instance RuntimeRepOf Text    = AsNative ('ClassRep "Std.Base" "Text")

instance FromObject () where
    fromConstructor _ = return ()
instance ToObject () where
    toConstructor _ _ = Luna.Constructor "None" []
type instance RuntimeRepOf () = AsClass () ('ClassRep "Std.Base" "None")
