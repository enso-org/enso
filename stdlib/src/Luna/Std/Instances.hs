module Luna.Std.Instances where

import Prologue

import qualified Control.Concurrent.Async as Async
import qualified Luna.Runtime             as Luna
import qualified OCI.Data.Name            as Name

import Control.Concurrent.MVar (MVar)
import Data.ByteString         (ByteString)
import Data.Vector             (Vector)

type BaseModule = "Std.Base"

baseModule :: Name.Qualified
baseModule = Name.qualFromSymbol @BaseModule

type instance Luna.RuntimeRepOf ByteString         = Luna.AsNative ('Luna.ClassRep BaseModule "Binary")
type instance Luna.RuntimeRepOf (MVar Luna.Data)   = Luna.AsNative ('Luna.ClassRep BaseModule "MVar")
type instance Luna.RuntimeRepOf (Vector Luna.Data) = Luna.AsNative ('Luna.ClassRep BaseModule "Vector")
type instance Luna.RuntimeRepOf (Async.Async (Either Luna.Exception Luna.Data)) =
    Luna.AsNative ('Luna.ClassRep BaseModule "Future")

type instance Luna.RuntimeRepOf Bool = Luna.AsClass Bool ('Luna.ClassRep BaseModule "Bool")
instance Luna.FromObject Bool where
    fromConstructor c = return $ c ^. Luna.tag == "True"
instance Luna.ToObject Bool where
    toConstructor _ b = Luna.Constructor (if b then "True" else "False") []

type instance Luna.RuntimeRepOf (a, b) = Luna.AsClass (a, b) ('Luna.ClassRep BaseModule "Tuple2")
instance (Luna.FromData a, Luna.FromData b) => Luna.FromObject (a, b) where
    fromConstructor (Luna.Constructor "Tuple2" [a, b]) = (,) <$> Luna.fromData a <*> Luna.fromData b
    fromConstructor (Luna.Constructor "Tuple2" _)      = Luna.throw "Malformed Tuple2 object."
    fromConstructor (Luna.Constructor n        _)      = Luna.throw $ "Expected a Tuple2 object, got " <> convert n
instance (Luna.ToData a, Luna.ToData b) => Luna.ToObject (a, b) where
    toConstructor imps (a, b) = Luna.Constructor "Tuple2" [Luna.toData imps a, Luna.toData imps b]

type instance Luna.RuntimeRepOf (a, b, c) = Luna.AsClass (a, b, c) ('Luna.ClassRep BaseModule "Tuple3")
instance (Luna.FromData a, Luna.FromData b, Luna.FromData c) => Luna.FromObject (a, b, c) where
    fromConstructor (Luna.Constructor "Tuple3" [a, b, c]) = (,,) <$> Luna.fromData a <*> Luna.fromData b <*> Luna.fromData c
    fromConstructor (Luna.Constructor "Tuple3" _)         = Luna.throw "Malformed Tuple3 object."
    fromConstructor (Luna.Constructor n        _)         = Luna.throw $ "Expected a Tuple3 object, got " <> convert n
instance (Luna.ToData a, Luna.ToData b, Luna.ToData c) => Luna.ToObject (a, b, c) where
    toConstructor imps (a, b, c) = Luna.Constructor "Tuple3" [Luna.toData imps a, Luna.toData imps b, Luna.toData imps c]

type instance Luna.RuntimeRepOf (a, b, c, d) = Luna.AsClass (a, b, c, d) ('Luna.ClassRep BaseModule "Tuple4")
instance (Luna.FromData a, Luna.FromData b, Luna.FromData c, Luna.FromData d) => Luna.FromObject (a, b, c, d) where
    fromConstructor (Luna.Constructor "Tuple4" [a, b, c, d]) = (,,,) <$> Luna.fromData a <*> Luna.fromData b <*> Luna.fromData c <*> Luna.fromData d
    fromConstructor (Luna.Constructor "Tuple4" _)            = Luna.throw "Malformed Tuple4 object."
    fromConstructor (Luna.Constructor n        _)            = Luna.throw $ "Expected a Tuple4 object, got " <> convert n
instance (Luna.ToData a, Luna.ToData b, Luna.ToData c, Luna.ToData d) => Luna.ToObject (a, b, c, d) where
    toConstructor imps (a, b, c, d) = Luna.Constructor "Tuple4" [Luna.toData imps a, Luna.toData imps b, Luna.toData imps c, Luna.toData imps d]

type instance Luna.RuntimeRepOf (Maybe a) = Luna.AsClass (Maybe a) ('Luna.ClassRep BaseModule "Maybe")
instance Luna.FromData a => Luna.FromObject (Maybe a) where
    fromConstructor (Luna.Constructor "Nothing" [])  = return Nothing
    fromConstructor (Luna.Constructor "Just"    [a]) = Just <$> Luna.fromData a
    fromConstructor _ = Luna.throw "Malformed Maybe object."
instance Luna.ToData a => Luna.ToObject (Maybe a) where
    toConstructor imps (Just a) = Luna.Constructor "Just" [Luna.toData imps a]
    toConstructor imps Nothing  = Luna.Constructor "Nothing" []

type instance Luna.RuntimeRepOf (Either a b) = Luna.AsClass (Either a b) ('Luna.ClassRep BaseModule "Either")
instance (Luna.FromData a, Luna.FromData b) => Luna.FromObject (Either a b) where
    fromConstructor (Luna.Constructor "Left"  [a]) = Left <$> Luna.fromData a
    fromConstructor (Luna.Constructor "Right" [b]) = Left <$> Luna.fromData b
    fromConstructor _ = Luna.throw "Malformed Either object."
instance (Luna.ToData a, Luna.ToData b) => Luna.ToObject (Either a b) where
    toConstructor imps (Left  a) = Luna.Constructor "Left"  [Luna.toData imps a]
    toConstructor imps (Right b) = Luna.Constructor "Right" [Luna.toData imps b]

type instance Luna.RuntimeRepOf [a] = Luna.AsClass [a] ('Luna.ClassRep BaseModule "List")
instance Luna.FromData a => Luna.FromObject [a] where
    fromConstructor (Luna.Constructor "Empty"   [])     = return []
    fromConstructor (Luna.Constructor "Prepend" [a, b]) = (:) <$> Luna.fromData a <*> Luna.fromData b
    fromConstructor _ = Luna.throw "Malformed List object."
instance Luna.ToData a => Luna.ToObject [a] where
    toConstructor _    []       = Luna.Constructor "Empty" []
    toConstructor imps (a : as) = Luna.Constructor "Prepend" [Luna.toData imps a, Luna.toData imps as]
