{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.IR.Layer.UserType where

import Luna.Prelude
import OCI.IR.Layer.Class
import OCI.IR.Class        (SubLink, AnyExpr, Import)
import OCI.Pass
import OCI.IR
import OCI.Pass.Definition (makePass)
import Luna.IR.ToRefactor2 (Listener, listener, addExprEventListener, tpElemPass)
import Type.Any (AnyType)
import Data.TypeDesc (getTypeDesc_, typeDesc)
import Luna.IR.Layer.Type

data UserType = UserType deriving (Show)
type instance LayerData UserType t = Maybe (SubLink Type t)

initUserType :: Req m '[Writer // Layer // AnyExpr // UserType] => Listener (New // Expr l) m
initUserType = listener $ \(t, _) -> putLayer @UserType t Nothing
makePass 'initUserType

watchUserTypeImport :: Req m '[Editor // Layer // AnyExpr // UserType] => Listener (Import // Expr l) m
watchUserTypeImport = listener $ \(t, trans, l) -> if getTypeDesc_ @UserType ^. from typeDesc `elem` l
    then putLayer     @UserType t Nothing
    else modifyLayer_ @UserType t $ fmap $ trans ^. linkTranslator
makePass 'watchUserTypeImport

init :: MonadPassManager m => m ()
init = do
    addExprEventListener @UserType initUserTypePass
    addExprEventListener @UserType watchUserTypeImportPass
