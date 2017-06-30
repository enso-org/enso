{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.IR.Layer.Requester where

import Luna.Prelude
import OCI.IR.Layer.Class
import OCI.IR.Class        (SubLink, AnyExpr, Import)
import OCI.Pass
import OCI.IR
import OCI.Pass.Definition (makePass)
import Luna.IR.ToRefactor2 (Listener, listener, addExprEventListener, tpElemPass)
import Type.Any (AnyType)
import Data.TypeDesc (getTypeDesc_, typeDesc)

data Requester = Requester deriving (Show)
type instance LayerData Requester t = Maybe (SubLink AnyExpr t)

initRequester :: Req m '[Writer // Layer // AnyExpr // Requester] => Listener (New // Expr l) m
initRequester = listener $ \(t, _) -> putLayer @Requester t Nothing
makePass 'initRequester

watchRequesterImport :: Req m '[Editor // Layer // AnyExpr // Requester] => Listener (Import // Expr l) m
watchRequesterImport = listener $ \(t, trans, l) -> if getTypeDesc_ @Requester ^. from typeDesc `elem` l
    then putLayer     @Requester t Nothing
    else modifyLayer_ @Requester t $ fmap $ trans ^. linkTranslator
makePass 'watchRequesterImport

init :: MonadPassManager m => m ()
init = do
    addExprEventListener @Requester initRequesterPass
    addExprEventListener @Requester watchRequesterImportPass
